module Main (main) where

import Control.Applicative ((<**>))
import Control.Category ((>>>))
import Data.Foldable (for_)
import Data.Int (Int32)
import Data.List ((!?))
import Data.Ord (clamp)
import Data.Traversable (for)

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Graphics.Text.Font.Choose qualified as FC
import Options.Applicative qualified as Opt
import SDL qualified
import SDL.Font qualified as TTF
import System.Directory qualified as Dir
import Toml qualified

import Pred.MVU (Destination (..), runMVU)
import Pred.Prelude
import Pred.Region (liftIO, region, (<...&>))

data InputEvent = KeyPress SDL.Keycode | MouseScroll (SDL.V2 Int32) | OtherEvent

data Config = MkConfig
  { fontPath :: Text
  , fontSize :: TTF.PointSize
  }
  deriving Generic

type ScrollPos = SDL.V2 Int32

data Toolkit = MkToolkit
  { window     :: SDL.Window
  , configPath :: FilePath
  , config     :: Config
  , filePath   :: FilePath
  , scrollPos  :: ScrollPos
  }
  deriving Generic

data State = Init | Idle | Saving

data Model :: State -> Type where
  MInit :: Model Init
  MIdle :: Toolkit -> Model Idle
  MSave :: FilePath -> Config -> Model Saving

data Event :: State -> Type where
  Ready :: Toolkit -> Event Init
  FontSizeChanged :: TTF.PointSize -> Event Idle
  ScrollPosChanged :: ScrollPos -> Event Idle
  Save :: Event Idle
  Quit :: Event Saving

main :: IO ()
main = region $ runMVU MInit routeTable \case
 MInit -> do
  filePath :: FilePath <- liftIO $ Opt.execParser $ Opt.info
    (Opt.strArgument
      (Opt.metavar "FILE" <> Opt.help "File to edit" <> Opt.action "file")
      <**> Opt.helper)
    (Opt.fullDesc <> Opt.progDesc ("PrEd is a Proof Editor, "
      <> "an IDE specifically tailored for interactive proof assistants."))
  SDL.initializeAll
  TTF.initialize <...&> const TTF.quit
  window <- SDL.createWindow "PrEd proof editor" SDL.defaultWindow
    { windowHighDPI = True
    , windowMode = SDL.Maximized
    , windowResizable = True
    } <...&> SDL.destroyWindow
  configPath <- liftIO (Dir.getXdgDirectory Dir.XdgConfig "predconfig.toml")
  config <- liftIO do
    configExists <- Dir.doesFileExist configPath
    if configExists
    then Toml.decodeFile Toml.genericCodec configPath
    else do
      fontPath <- do
        fc <- FC.initLoadConfigAndFonts
        pattern <- maybe (error "lol no Fira Code") pure $
          FC.fontMatch fc (FC.nameParse "Fira Code")
        maybe (error "lol no filepath") (pure . Text.pack) $
          FC.getValue "file" pattern
      pure MkConfig { fontSize = 36, .. }
  pure $ Ready MkToolkit { scrollPos = SDL.V2 0 0, .. }
 MIdle MkToolkit { config = MkConfig {..}, .. } -> region do
  font <- TTF.load (Text.unpack fontPath) fontSize <...&> TTF.free
  textLines <- liftIO do
    fileExists <- Dir.doesFileExist filePath
    if fileExists
    then Text.lines <$> Text.readFile filePath
    else pure []
  fontSurfaces <- for textLines \line ->
    if Text.null line
    then pure Nothing
    else do
      surface <- TTF.solid font (SDL.V4 255 255 255 255) line
        <...&> SDL.freeSurface
      pure (Just surface)
  liftIO $ fix \retry -> do
    windowSurface <- SDL.getWindowSurface window
    SDL.surfaceFillRect windowSurface Nothing (SDL.V4 0 0 0 255)
    lineSkip <- TTF.lineSkip font
    let SDL.V2 colPos linePos = scrollPos
    colSkip <- case textLines !? fromIntegral linePos of
      Nothing -> pure 0
      Just line -> do
        let pos = fromIntegral colPos
            start = Text.take pos line
        (trueWidth, _) <- TTF.size font (Text.take pos line)
        Just (_, _, _, _, advance) <- TTF.glyphMetrics font 'o'
        pure $ trueWidth + advance * max 0 (pos - Text.length start)
    for_ (zip [0..] fontSurfaces) \(i, fs) -> case fs of
      Just fontSurface ->
        SDL.surfaceBlit fontSurface Nothing windowSurface . Just . SDL.P $
          SDL.V2 (- toEnum colSkip)
                 ((i - fromIntegral linePos) * toEnum lineSkip)
      Nothing -> pure Nothing
    SDL.updateWindowSurface window
    event <- SDL.waitEvent
    case interestingEvent event of
      KeyPress SDL.KeycodeQ      -> pure Save
      KeyPress SDL.KeycodeEquals -> pure $ FontSizeChanged (fontSize + 1)
      KeyPress SDL.KeycodeMinus  -> pure $ FontSizeChanged (fontSize - 1)
      MouseScroll (SDL.V2 dx dy) -> pure $ ScrollPosChanged $
        SDL.V2 (clamp (0, fromIntegral $ maximum $ map Text.length textLines)
                      (colPos + dx))
               (clamp (0, fromIntegral $ length textLines) (linePos - dy))
      _                          -> retry
 MSave configPath config -> liftIO do
  Quit <$ Toml.encodeToFile Toml.genericCodec configPath config
 where
  routeTable :: Event s -> Model s -> Destination Model ()
  routeTable = \case
    Ready tk -> \_ -> To (MIdle tk)
    FontSizeChanged fontSize -> \(MIdle tk) -> To $ MIdle tk { config.fontSize }
    ScrollPosChanged scrollPos -> \(MIdle tk) -> To $ MIdle tk { scrollPos }
    Save -> \(MIdle MkToolkit {..}) -> To $ MSave configPath config
    Quit -> \_ -> Exit ()

  interestingEvent = SDL.eventPayload >>> \case
    SDL.KeyboardEvent keyboardEvent
      | keyboardEvent.keyboardEventKeyMotion == SDL.Pressed ->
        KeyPress keyboardEvent.keyboardEventKeysym.keysymKeycode
    SDL.MouseWheelEvent wheelEvent -> MouseScroll wheelEvent.mouseWheelEventPos
    _ -> OtherEvent
