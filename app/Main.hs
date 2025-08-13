module Main (main) where

import Control.Applicative ((<**>))
import Control.Category ((>>>))
import Control.Monad (void)
import Data.Foldable (fold, for_)
import Data.Functor ((<&>))
import Data.Int (Int32)
import Data.List ((!?))
import Data.Maybe (catMaybes)
import Data.Ord (clamp)
import Data.Traversable (for)
import Foreign.C (CInt)
import System.Exit (exitSuccess)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource qualified as Resource
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Graphics.Text.Font.Choose qualified as FC
import Options.Applicative qualified as Opt
import Reactive.Banana qualified as Banana
import Reactive.Banana.Frameworks qualified as Banana
import SDL qualified
import SDL.Font qualified as TTF
import System.Directory qualified as Dir
import Toml qualified

import Pred.Prelude

data InputEvent = KeyPress SDL.Keycode | MouseScroll (SDL.V2 Int32) | OtherEvent

data Config = MkConfig
  { fontPath :: Text
  , fontSize :: TTF.PointSize
  }
  deriving Generic

data FontData m = MkFontData
  { font         :: TTF.Font
  , fontSurfaces :: [(CInt, SDL.Surface)]
  , recreate     :: (Config -> Config) -> m (FontData m)
  , saveConfig   :: FilePath -> m ()
  }
  deriving Generic

newFontData ::
  Resource.MonadResource m => Config -> [Text] -> m (FontData m)
newFontData config textLines = do
  (fontRK, font) <- TTF.load (Text.unpack config.fontPath) config.fontSize
                      `Resource.allocate` TTF.free
  (surfacesRK, fontSurfaces) <- unzip . catMaybes <$> for (zip [0..] textLines)
    \(lineNum, line) ->
      if Text.null line
      then pure Nothing
      else Just . fmap (lineNum ,) <$>
        (TTF.solid font (SDL.V4 255 255 255 255) line
          `Resource.allocate` SDL.freeSurface)
  let freeFont = Resource.release fontRK >> for_ surfacesRK Resource.release
      recreate update = freeFont >> newFontData (update config) textLines
      saveConfig configPath =
        freeFont >> void (Toml.encodeToFile Toml.genericCodec configPath config)
  pure MkFontData {..}

banana ::
  SDL.Window -> Text -> FilePath -> TTF.PointSize ->
  Banana.AddHandler SDL.Event -> Banana.MomentIO ()
banana window text fontPath initialFontSize handler = do
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
                            (length textLines)
  exitKeyOnce <- Banana.once exitKey
  let exit = exitSuccess <$ exitKeyOnce
  position <- Banana.accumB (SDL.V2 0 0) $ scroll <&> updateSP scrollBounds
  fontSize <- Banana.accumE initialFontSize $ fmap (+) resize
  font <- Banana.mapEventIO (TTF.load fontPath) fontSize
  textSurfaces <- Banana.mapEventIO (for textLines . traverse . renderLine) font
  fontB <- Banana.stepper Nothing (Just <$> font)
  textSurfacesB <- Banana.stepper Nothing (Just <$> textSurfaces)
  let oldFonts = fontSize Banana.@> fontB
      oldSurfaces = font Banana.@> textSurfacesB
      freeOldFonts = foldMap TTF.free <$> oldFonts
      freeOldSurfaces =
        foldMap (foldMap $ SDL.freeSurface . snd) <$> oldSurfaces
      lineSkip = TTF.lineSkip <$> font
      render = renderAll window <$> position Banana.<@> textSurfaces
  Banana.reactimate $ fold
    [ freeOldFonts
    , freeOldSurfaces
    , exit
    ]
  where
    updateSP (SDL.V2 maxX maxY) (SDL.V2 dx dy) (SDL.V2 x y) = SDL.V2
      (clamp (0, maxX) (x + dx)) (clamp (0, maxY) (y - dy))
    renderLine = (`TTF.solid` SDL.V4 255 255 255 255)
    renderAll ww (SDL.V2 colPos rowPos) surfaces = _

main :: IO ()
main = Resource.runResourceT do
  filePath <- liftIO $ Opt.execParser $ Opt.info
    (Opt.strArgument
      (Opt.metavar "FILE" <> Opt.help "File to edit" <> Opt.action "file")
        <**> Opt.helper)
    (Opt.fullDesc <> Opt.progDesc ("PrEd is a Proof Editor, "
      <> "an IDE specifically tailored for interactive proof assistants."))
  textLines <- liftIO $ Dir.doesFileExist filePath >>= \case
    True -> Text.lines <$> Text.readFile filePath
    False -> pure []
  SDL.initializeAll
  _ <- TTF.initialize `Resource.allocate_` TTF.quit
  (_, window) <- SDL.createWindow "PrEd proof editor" SDL.defaultWindow
    { windowHighDPI = True
    , windowMode = SDL.Maximized
    , windowResizable = True
    } `Resource.allocate` SDL.destroyWindow
  configPath <- liftIO $ Dir.getXdgDirectory Dir.XdgConfig "predconfig.toml"
  fd <- do
    config <- liftIO $ Dir.doesFileExist configPath >>= \case
      True -> Toml.decodeFile Toml.genericCodec configPath
      False -> do
        fc <- FC.initLoadConfigAndFonts
        pattern <- maybe (error "lol no Fira Code") pure $
          FC.fontMatch fc (FC.nameParse "Fira Code")
        fontPath <- maybe (error "lol no filepath") (pure . Text.pack) $
          FC.getValue "file" pattern
        pure MkConfig { fontSize = 36, .. }
    newFontData config textLines
  flip fix (fd, SDL.V2 0 0) \loop (fontData, scrollPos) -> do
    windowSurface <- SDL.getWindowSurface window
    SDL.surfaceFillRect windowSurface Nothing (SDL.V4 0 0 0 255)
    lineSkip <- toEnum <$> TTF.lineSkip fontData.font
    let SDL.V2 colPos linePos = scrollPos
    colSkip <- case textLines !? fromIntegral linePos of
      Nothing -> pure 0
      Just line -> do
        let pos = fromIntegral colPos
            start = Text.take pos line
        (trueWidth, _) <- TTF.size fontData.font (Text.take pos line)
        Just (_, _, _, _, advance) <- TTF.glyphMetrics fontData.font 'o'
        pure $ trueWidth + advance * max 0 (pos - Text.length start)
    SDL.V2 _ windowHeight <- SDL.surfaceDimensions windowSurface
    for_ fontData.fontSurfaces \(i, fontSurface) -> do
      let blitY = (i - fromIntegral linePos) * lineSkip
          blitPos = SDL.V2 (-toEnum colSkip) blitY
      if 0 <= blitY && blitY < windowHeight
      then SDL.surfaceBlit fontSurface Nothing windowSurface $
        Just (SDL.P blitPos)
      else pure Nothing
    SDL.updateWindowSurface window
    event <- SDL.waitEvent
    case interestingEvent event of
      KeyPress SDL.KeycodeQ      -> fontData.saveConfig configPath
      KeyPress SDL.KeycodeEquals -> do
        fontData' <- fontData.recreate \config -> config
          { fontSize = config.fontSize + 1 }
        loop (fontData', scrollPos)
      KeyPress SDL.KeycodeMinus  -> do
        fontData' <- fontData.recreate \config -> config
          { fontSize = config.fontSize - 1 }
        loop (fontData', scrollPos)
      MouseScroll (SDL.V2 dx dy) -> do
        let scrollPos' = SDL.V2
              (clamp (0, fromIntegral $ maximum $ map Text.length textLines)
                     (colPos + dx))
              (clamp (0, fromIntegral $ length textLines)
                     (linePos - dy))
        loop (fontData, scrollPos')
      _                          -> loop (fontData, scrollPos)
 where
  interestingEvent = SDL.eventPayload >>> \case
    SDL.KeyboardEvent keyboardEvent
      | keyboardEvent.keyboardEventKeyMotion == SDL.Pressed ->
        KeyPress keyboardEvent.keyboardEventKeysym.keysymKeycode
    SDL.MouseWheelEvent wheelEvent -> MouseScroll wheelEvent.mouseWheelEventPos
    _ -> OtherEvent
