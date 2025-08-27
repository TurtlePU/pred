module Main (main) where

import Control.Applicative ((<**>))
import Control.Monad (forever, when)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Ord (clamp)
import Data.Word (Word32)
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
import Data.Maybe (fromMaybe)

data Mode = Normal | Edit

main :: IO ()
main = Resource.runResourceT do
  SDL.initializeAll
  (_, window) <- SDL.createWindow "PrEd proof editor" SDL.defaultWindow
    { windowHighDPI = True
    , windowMode = SDL.Maximized
    , windowResizable = True
    } `Resource.allocate` SDL.destroyWindow
  (_, fonts) <- TTF.newFonts `Resource.allocate` TTF.closeFonts
  liftIO do
    (sdlHandler, fireSDL) <- Banana.newAddHandler
    (timerHandler, fireTimer) <- Banana.newAddHandler
    Banana.compile (banana window fonts sdlHandler timerHandler)
      >>= Banana.actuate
    forever do
      SDL.waitEventTimeout 16 >>= maybe mempty fireSDL
      SDL.ticks >>= fireTimer

banana ::
  SDL.Window -> TTF.Fonts ->
  Banana.AddHandler SDL.Event -> Banana.AddHandler Word32 -> Banana.MomentIO ()
banana window fonts sdlHandler timerHandler = do
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
  sdlE <- Banana.fromAddHandler sdlHandler
  let (press, scroll) = Banana.split $ Banana.filterJust $ sdlE <&> \e ->
        case e.eventPayload of
          SDL.KeyboardEvent ked
            | ked.keyboardEventKeyMotion == SDL.Pressed ->
              Just (Left ked.keyboardEventKeysym.keysymKeycode)
          SDL.MouseWheelEvent mwed -> Just $ Right
            (fromIntegral <$> mwed.mouseWheelEventPos)
          _ -> Nothing
      clicks = Banana.filterJust $ sdlE <&> \e ->
        case e.eventPayload of
          SDL.MouseButtonEvent mbed
            | mbed.mouseButtonEventMotion == SDL.Pressed
              && mbed.mouseButtonEventWindow == Just window ->
                Just (fromIntegral <$> mbed.mouseButtonEventPos)
          _ -> Nothing
      modeE = Banana.filterJust $ press <&> \case
        SDL.KeycodeEscape -> Just Normal
        SDL.KeycodeReturn -> Just Edit
        _ -> Nothing
      (exitKey, resize) = Banana.split $ Banana.filterJust $ press <&> \case
        SDL.KeycodeQ -> Just (Left ())
        SDL.KeycodeEquals -> Just (Right 1)
        SDL.KeycodeMinus -> Just (Right (-1))
        _ -> Nothing
      textLines = filter (not . Text.null . snd) $ zip [0..] (Text.lines text)
      scrollBounds = SDL.V2 (maximum (0 : map (Text.length . snd) textLines))
                            (maximum (0 : map (succ . fst) textLines))
  time <- Banana.fromAddHandler timerHandler >>= Banana.stepper 0
  lastClickTime <- Banana.stepper 0 $ clicks Banana.@> time
  let drawCursor = (\t lct -> (t - lct) `mod` 1000 < 500) <$> time <*> lastClickTime
  position <- Banana.accumB (SDL.V2 0 0) $ scroll <&> updateSP scrollBounds
  font <- Banana.accumB initialFont $ resize <&>
    \ds font -> font { pointSize = font.pointSize + ds }
  clickPos <- Banana.mapEventIO id $
    findClickPos textLines <$> position <*> font Banana.<@> clicks
  click <- Banana.stepper (SDL.V2 0 0) clickPos
  mode <- Banana.stepper Normal modeE
  let renderer =
        renderAll textLines <$> position
                            <*> font
                            <*> drawCursor
                            <*> click
                            <*> mode
  Banana.changes renderer >>= Banana.reactimate'
  Banana.valueB renderer >>= liftIO
  onceExit <- Banana.once exitKey
  Banana.reactimate $ onceExit Banana.@> font <&> \f -> do
    _ <- Toml.encodeToFile Toml.genericCodec configPath f
    exitSuccess
  where
    updateSP (SDL.V2 maxX maxY) (SDL.V2 dx dy) (SDL.V2 x y) = SDL.V2
      (clamp (0, maxX) (x + dx)) (clamp (0, maxY) (y - dy))
    findClickPos tLines (SDL.V2 colPos linePos) font (SDL.P (SDL.V2 cx cy)) = do
      fc <- TTF.load fonts font
      lineSkip <- TTF.lineSkip fc
      let lineCPos = linePos + cy `div` lineSkip
          line = fromMaybe "" $ lineCPos `lookup` tLines
      colCPos <- binarySearch (-1, Text.length line) \i -> do
        (widthL, _) <- TTF.size fc $ Text.drop colPos $ Text.take (max 0 i) line
        (widthR, _) <- TTF.size fc $ Text.drop colPos $ Text.take (i + 1) line
        pure $ (widthL + widthR) `div` 2 > cx
      pure $ SDL.V2 colCPos lineCPos
    renderAll textLines (SDL.V2 colPos linePos) font drawCursor
                        (SDL.V2 colCPos lineCPos) mode = do
      windowSurface <- SDL.getWindowSurface window
      SDL.surfaceFillRect windowSurface Nothing (SDL.V4 0 0 0 255)
      fontCache <- TTF.load fonts font
      lineSkip <- toEnum <$> TTF.lineSkip fontCache
      Just (_, _, _, _, advance) <- TTF.glyphMetrics fontCache 'o'
      colSkip <- do
        let pos = fromIntegral colPos
            start = case lookup (fromIntegral linePos) textLines of
              Nothing   -> ""
              Just line -> Text.take pos line
        (trueWidth, _) <- TTF.size fontCache start
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
      let clickY = fromIntegral (lineCPos - linePos) * lineSkip
      when (clickY >= 0 && colCPos >= colPos) do
        (clickX, _) <- TTF.size fontCache
                        $ Text.drop colPos
                        $ Text.take colCPos
                        $ fromMaybe ""
                        $ lookup lineCPos textLines
        let rectPos = SDL.P $ fromIntegral clickX `SDL.V2` clickY
        case mode of
          Normal -> do
            let char = fromMaybe ' ' $
                  lookup lineCPos textLines >>= safeIndex colCPos
            Just (_, _, _, _, rectWidth) <- TTF.glyphMetrics fontCache char
            let rect = SDL.Rectangle rectPos
                  (toEnum rectWidth `SDL.V2` lineSkip)
            SDL.surfaceFillRect windowSurface (Just rect) (SDL.V4 255 255 255 255)
            charSurface <- TTF.solid fontCache (SDL.V4 0 0 0 255)
                                               (Text.singleton char)
            _ <- SDL.surfaceBlit charSurface Nothing windowSurface $ Just rectPos
            pure ()
          Edit -> when drawCursor do
            let rect = SDL.Rectangle rectPos
                  (SDL.V2 (toEnum advance `div` 5) lineSkip)
            SDL.surfaceFillRect windowSurface (Just rect) (SDL.V4 255 255 255 255)
      SDL.updateWindowSurface window

binarySearch :: (Integral a, Monad m) => (a, a) -> (a -> m Bool) -> m a
binarySearch (start, end) test = go start end
  where
    go lo hi
      | succ lo >= hi = pure hi
      | otherwise = do
         let mid = lo + (hi - lo) `div` 2
         test mid >>= \case
           True -> go lo mid
           False -> go mid hi

safeIndex :: Int -> Text.Text -> Maybe Char
safeIndex i t
  | i < Text.length t = Just $ Text.index t i
  | otherwise = Nothing
