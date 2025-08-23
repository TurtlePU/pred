module Main (main) where

import Control.Applicative ((<**>))
import Control.Monad (forever)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Ord (clamp)
import System.Exit (exitSuccess)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource qualified as Resource
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Graphics.Text.Font.Choose qualified as FC
import Options.Applicative qualified as Opt
import Reactive.Banana qualified as Banana
import Reactive.Banana.Frameworks qualified as Banana
import SDL qualified
import System.Directory qualified as Dir
import Toml qualified

import Pred.Prelude
import Pred.TTF qualified as TTF

main :: IO ()
main = Resource.runResourceT do
  (_, window) <- SDL.createWindow "PrEd proof editor" SDL.defaultWindow
    { windowHighDPI = True
    , windowMode = SDL.Maximized
    , windowResizable = True
    } `Resource.allocate` SDL.destroyWindow
  (_, fonts) <- TTF.newFonts `Resource.allocate` TTF.closeFonts
  liftIO do
    (addHandler, fire) <- Banana.newAddHandler
    network <- Banana.compile (banana window fonts addHandler)
    Banana.actuate network
    forever (SDL.waitEvent >>= fire)

banana ::
  SDL.Window -> TTF.Fonts -> Banana.AddHandler SDL.Event -> Banana.MomentIO ()
banana window fonts handler = do
  filePath <- liftIO $ Opt.execParser $ Opt.info
    (Opt.strArgument
      (Opt.metavar "FILE" <> Opt.help "File to edit" <> Opt.action "file")
        <**> Opt.helper)
    (Opt.fullDesc <> Opt.progDesc ("PrEd is a Proof Editor, "
      <> "an IDE specifically tailored for interactive proof assistants."))
  text <- liftIO $ Dir.doesFileExist filePath >>= \case
    True -> Text.readFile filePath
    False -> pure ""
  configPath <- liftIO $ Dir.getXdgDirectory Dir.XdgConfig "predconfig.toml"
  initialFont <- liftIO $ Dir.doesFileExist configPath >>= \case
    True -> Toml.decodeFile Toml.genericCodec configPath
    False -> do
      fc <- FC.initLoadConfigAndFonts
      pattern <- maybe (error "lol no Fira Code") pure $
        FC.fontMatch fc (FC.nameParse "Fira Code")
      path <- maybe (error "lol no filepath") (pure . Text.pack) $
        FC.getValue "file" pattern
      pure TTF.MkFont { pointSize = 36, .. }
  SDL.initializeAll
  sdlE <- Banana.fromAddHandler handler
  let (press, scroll) = Banana.split $ Banana.filterJust $ sdlE <&> \e ->
        case e.eventPayload of
          SDL.KeyboardEvent ked
            | ked.keyboardEventKeyMotion == SDL.Pressed ->
              Just (Left ked.keyboardEventKeysym.keysymKeycode)
          SDL.MouseWheelEvent mwed -> Just $ Right
            (fromIntegral <$> mwed.mouseWheelEventPos)
          _ -> Nothing
      (exitKey, resize) = Banana.split $ Banana.filterJust $ press <&> \case
        SDL.KeycodeQ -> Just (Left ())
        SDL.KeycodeEquals -> Just (Right 1)
        SDL.KeycodeMinus -> Just (Right (-1))
        _ -> Nothing
      textLines = filter (not . Text.null . snd) $ zip [0..] (Text.lines text)
      scrollBounds = SDL.V2 (maximum (0 : map (Text.length . snd) textLines))
                            (maximum (0 : map (succ . fst) textLines))
      initialPos = SDL.V2 0 0
  position <- Banana.accumB initialPos $ scroll <&> updateSP scrollBounds
  font <- Banana.accumB initialFont $ resize <&>
    \ds font -> font { pointSize = font.pointSize + ds }
  render <- Banana.changes $ renderAll textLines <$> position <*> font
  Banana.reactimate' render
  liftIO (renderAll textLines initialPos initialFont)
  onceExit <- Banana.once exitKey
  Banana.reactimate $ onceExit Banana.@> font <&> \f -> do
    _ <- Toml.encodeToFile Toml.genericCodec configPath f
    exitSuccess
  where
    updateSP (SDL.V2 maxX maxY) (SDL.V2 dx dy) (SDL.V2 x y) = SDL.V2
      (clamp (0, maxX) (x + dx)) (clamp (0, maxY) (y - dy))
    renderAll textLines (SDL.V2 colPos linePos) font = do
      windowSurface <- SDL.getWindowSurface window
      SDL.surfaceFillRect windowSurface Nothing (SDL.V4 0 0 0 255)
      fontCache <- TTF.load fonts font
      lineSkip <- toEnum <$> TTF.lineSkip fontCache
      colSkip <- do
        let pos = fromIntegral colPos
            start = case lookup (fromIntegral linePos) textLines of
              Nothing -> ""
              Just line -> Text.take pos line
        (trueWidth, _) <- TTF.size fontCache start
        Just (_, _, _, _, advance) <- TTF.glyphMetrics fontCache 'o'
        pure $ trueWidth + advance * max 0 (pos - Text.length start)
      SDL.V2 _ windowHeight <- SDL.surfaceDimensions windowSurface
      for_ textLines \(i, line) -> do
        let blitY = toEnum (i - linePos) * lineSkip
            blitPos = SDL.V2 (-toEnum colSkip) blitY
        if 0 <= blitY && blitY < windowHeight
        then do
          lineSurface <- TTF.solid fontCache (SDL.V4 255 255 255 255) line
          SDL.surfaceBlit lineSurface Nothing windowSurface $
            Just (SDL.P blitPos)
        else pure Nothing
      SDL.updateWindowSurface window
