module Main (main) where

import Control.Applicative ((<**>))
import Control.Category ((>>>))
import Control.Monad (void)
import Data.Foldable (for_)
import Data.Int (Int32)
import Data.List ((!?))
import Data.Maybe (catMaybes)
import Data.Ord (clamp)
import Data.Traversable (for)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource qualified as Resource
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Graphics.Text.Font.Choose qualified as FC
import Options.Applicative qualified as Opt
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
  configPath <- liftIO $ Dir.getXdgDirectory Dir.XdgConfig "predconfig.toml"
  initialConfig <- liftIO $ Dir.doesFileExist configPath >>= \case
    True -> Toml.decodeFile Toml.genericCodec configPath
    False -> do
      fc <- FC.initLoadConfigAndFonts
      pattern <- maybe (error "lol no Fira Code") pure $
        FC.fontMatch fc (FC.nameParse "Fira Code")
      fontPath <- maybe (error "lol no filepath") (pure . Text.pack) $
        FC.getValue "file" pattern
      pure MkConfig { fontSize = 36, .. }
  _ <- TTF.initialize `Resource.allocate_` TTF.quit
  (_, window) <- SDL.createWindow "PrEd proof editor" SDL.defaultWindow
    { windowHighDPI = True
    , windowMode = SDL.Maximized
    , windowResizable = True
    } `Resource.allocate` SDL.destroyWindow
  flip fix initialConfig \changeConfig config -> do
    (fontRK, font) <- TTF.load (Text.unpack config.fontPath) config.fontSize
                        `Resource.allocate` TTF.free
    (surfacesRK, fontSurfaces) <- unzip . catMaybes <$>
      for (zip [0..] textLines) \(lineNum, line) ->
        if Text.null line
        then pure Nothing
        else Just . fmap (lineNum ,) <$>
          (TTF.solid font (SDL.V4 255 255 255 255) line
            `Resource.allocate` SDL.freeSurface)
    let freeFont = Resource.release fontRK >> for_ surfacesRK Resource.release
    flip fix (SDL.V2 0 0) \loop (SDL.V2 colPos linePos) -> do
      windowSurface <- SDL.getWindowSurface window
      SDL.surfaceFillRect windowSurface Nothing (SDL.V4 0 0 0 255)
      lineSkip <- toEnum <$> TTF.lineSkip font
      colSkip <- case textLines !? fromIntegral linePos of
        Nothing -> pure 0
        Just line -> do
          let pos = fromIntegral colPos
              start = Text.take pos line
          (trueWidth, _) <- TTF.size font (Text.take pos line)
          Just (_, _, _, _, advance) <- TTF.glyphMetrics font 'o'
          pure $ trueWidth + advance * max 0 (pos - Text.length start)
      SDL.V2 _ windowHeight <- SDL.surfaceDimensions windowSurface
      for_ fontSurfaces \(i, fontSurface) -> do
        let blitY = (i - fromIntegral linePos) * lineSkip
            blitPos = SDL.V2 (-toEnum colSkip) blitY
        if 0 <= blitY && blitY < windowHeight
        then SDL.surfaceBlit fontSurface Nothing windowSurface $
          Just (SDL.P blitPos)
        else pure Nothing
      SDL.updateWindowSurface window
      event <- SDL.waitEvent
      case interestingEvent event of
        KeyPress SDL.KeycodeQ      ->
          void (Toml.encodeToFile Toml.genericCodec configPath config)
        KeyPress SDL.KeycodeEquals -> freeFont >> changeConfig config
          { fontSize = config.fontSize + 1 }
        KeyPress SDL.KeycodeMinus  -> freeFont >> changeConfig config
          { fontSize = config.fontSize - 1 }
        MouseScroll (SDL.V2 dx dy) -> loop $ SDL.V2
          (clamp (0, fromIntegral $ maximum $ map Text.length textLines)
                 (colPos + dx))
          (clamp (0, fromIntegral $ length textLines)
                 (linePos - dy))
        _                          -> loop (SDL.V2 colPos linePos)
 where
  interestingEvent = SDL.eventPayload >>> \case
    SDL.KeyboardEvent keyboardEvent
      | keyboardEvent.keyboardEventKeyMotion == SDL.Pressed ->
        KeyPress keyboardEvent.keyboardEventKeysym.keysymKeycode
    SDL.MouseWheelEvent wheelEvent -> MouseScroll wheelEvent.mouseWheelEventPos
    _ -> OtherEvent
