module Main (main) where

import Control.Category ((>>>))
import System.Exit (exitSuccess)

import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Text.Font.Choose qualified as FC
import SDL qualified
import SDL.Font qualified as TTF
import System.Directory qualified as Dir
import Toml qualified

import Pred.MVU (Destination (..), runMVU)
import Pred.Prelude
import Pred.Region (liftIO, region, (<...&>))

data State = Init | Idle | Term

data Config = MkConfig
  { fontPath :: Text
  , fontSize :: TTF.PointSize
  }
  deriving Generic

data Toolkit = MkToolkit
  { window     :: SDL.Window
  , configPath :: FilePath
  , config     :: Config
  }
  deriving Generic

data Model :: State -> Type where
  MInit :: Model Init
  MIdle :: Toolkit -> Model Idle
  MSave :: FilePath -> Config -> Model Term

data Event :: State -> Type where
  Ready :: Toolkit -> Event Init
  FontSizeChanged :: TTF.PointSize -> Event Idle
  Quit :: Event Idle

main :: IO ()
main = region $ runMVU MInit routeTable \case
 MInit -> do
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
      let fontSize = 36
      pure MkConfig {..}
  pure $ Ready MkToolkit {..}
 MIdle MkToolkit { config = MkConfig {..}, .. } -> region do
  font <- TTF.load (Text.unpack fontPath) fontSize <...&> TTF.free
  fontSurface <- TTF.solid font (SDL.V4 255 255 255 255) "lol"
    <...&> SDL.freeSurface
  liftIO $ fix \retry -> do
    windowSurface <- SDL.getWindowSurface window
    SDL.surfaceFillRect windowSurface Nothing (SDL.V4 0 0 0 255)
    _ <- SDL.surfaceBlit fontSurface Nothing windowSurface Nothing
    SDL.updateWindowSurface window
    event <- SDL.waitEvent
    case eventPressKeyCode event of
      Just SDL.KeycodeQ      -> pure Quit
      Just SDL.KeycodeEquals -> pure $ FontSizeChanged (fontSize + 1)
      Just SDL.KeycodeMinus  -> pure $ FontSizeChanged (fontSize - 1)
      _                      -> retry
 MSave configPath config -> liftIO do
  _ <- Toml.encodeToFile Toml.genericCodec configPath config
  exitSuccess
 where
  routeTable :: Event s -> Model s -> Destination Model
  routeTable = \case
    Ready tk -> \_ -> To (MIdle tk)
    FontSizeChanged fontSize -> \(MIdle tk) -> To $ MIdle tk { config.fontSize }
    Quit -> \(MIdle MkToolkit {..}) -> To $ MSave configPath config

  eventPressKeyCode = SDL.eventPayload >>> \case
    SDL.KeyboardEvent keyboardEvent
      | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed ->
        Just $ SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)
    _ -> Nothing
