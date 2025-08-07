module Main (main) where

import Control.Category ((>>>))
import Control.Exception (bracket)
import Control.Monad.Managed (Managed, liftIO, managed, runManaged, with)
import Data.Function (fix)
import Data.Kind (Type)
import Graphics.Text.Font.Choose qualified as FC
import SDL qualified
import SDL.Font qualified as TTF
import System.Exit (exitSuccess)

import Pred.MVU (Destination, runMVU, to)

data State = Init | Idle

data Toolkit = MkToolkit
  { tkWindow   :: SDL.Window
  , tkFontPath :: FilePath
  , tkFontSize :: TTF.PointSize
  }

data Model :: State -> Type where
  MInit :: Model Init
  MIdle :: Toolkit -> Model Idle

data Event :: State -> Type where
  Ready :: Toolkit -> Event Init
  FontSizeChanged :: TTF.PointSize -> Event Idle

main :: IO ()
main = runManaged $ runMVU MInit routeTable \case
 MInit -> do
  SDL.initializeAll
  tkWindow <- SDL.createWindow "PrEd proof editor" SDL.defaultWindow
    { SDL.windowHighDPI = True
    , SDL.windowMode = SDL.Maximized
    , SDL.windowResizable = True
    } <...&> SDL.destroyWindow
  TTF.initialize <...&> const TTF.quit
  tkFontPath <- liftIO do
    fc <- FC.initLoadConfigAndFonts
    pattern <- maybe (error "lol no Fira Code") pure $
      FC.fontMatch fc (FC.nameParse "Fira Code")
    maybe (error "lol no filepath") pure $
      FC.getValue "file" pattern
  let tkFontSize = 36
  pure $ Ready MkToolkit {..}
 MIdle MkToolkit {..} -> liftIO do
  (do
    font <- TTF.load tkFontPath tkFontSize <...&> TTF.free
    TTF.solid font (SDL.V4 255 255 255 255) "lol" <...&> SDL.freeSurface
    ) `with` \fontSurface -> fix \retry -> do
    windowSurface <- SDL.getWindowSurface tkWindow
    SDL.surfaceFillRect windowSurface Nothing (SDL.V4 0 0 0 255)
    _ <- SDL.surfaceBlit fontSurface Nothing windowSurface Nothing
    SDL.updateWindowSurface tkWindow
    event <- SDL.waitEvent
    case eventPressKeyCode event of
      Just SDL.KeycodeQ -> exitSuccess
      Just SDL.KeycodeEquals -> pure $ FontSizeChanged (tkFontSize + 1)
      Just SDL.KeycodeMinus -> pure $ FontSizeChanged (tkFontSize - 1)
      _ -> retry
 where
  routeTable :: Event s -> Model s -> Destination Model
  routeTable = \case
    Ready tk -> to (MIdle tk)
    FontSizeChanged fs -> \(MIdle tk) r -> r $ MIdle tk { tkFontSize = fs }

  (<...&>) :: IO a -> (a -> IO b) -> Managed a
  create <...&> destroy = managed (bracket create destroy)

  eventPressKeyCode = SDL.eventPayload >>> \case
    SDL.KeyboardEvent keyboardEvent
      | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed ->
        Just $ SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)
    _ -> Nothing
