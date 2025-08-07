module Main (main) where

import Control.Category ((>>>))
import Control.Exception (bracket)
import Control.Monad.Managed (Managed, liftIO, managed, runManaged)
import Data.Function (fix)
import Data.Kind (Type)
import Graphics.Text.Font.Choose qualified as FC
import SDL qualified
import SDL.Font qualified as TTF
import System.Exit (exitSuccess)

import Pred.MVU (Destination, runMVU, to)

data State = Init | Idle

data Toolkit = MkToolkit
  { tkWindow      :: SDL.Window
  , tkFontSurface :: SDL.Surface
  }

data Model :: State -> Type where
  MInit :: Model Init
  MIdle :: Toolkit -> Model Idle

data Event :: State -> Type where
  Ready :: Toolkit -> Event Init

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
  fontPath <- liftIO do
    fc <- FC.initLoadConfigAndFonts
    pattern <- maybe (error "lol no Fira Code") pure $
      FC.fontMatch fc (FC.nameParse "Fira Code")
    maybe (error "lol no filepath") pure $
      FC.getValue "file" pattern
  font <- TTF.load fontPath 36 <...&> TTF.free
  tkFontSurface <- TTF.solid font (SDL.V4 255 255 255 255) "lol"
    <...&> SDL.freeSurface
  pure $ Ready MkToolkit {..}
 MIdle MkToolkit {..} -> liftIO $ fix \retry -> do
  windowSurface <- SDL.getWindowSurface tkWindow
  SDL.surfaceFillRect windowSurface Nothing (SDL.V4 0 0 0 255)
  _ <- SDL.surfaceBlit tkFontSurface Nothing windowSurface Nothing
  SDL.updateWindowSurface tkWindow
  event <- SDL.waitEvent
  if eventIsQPress event then exitSuccess else retry
 where
  routeTable :: Event s -> Model s -> Destination Model
  routeTable = \case
    Ready tk -> to (MIdle tk)

  (<...&>) :: IO a -> (a -> IO b) -> Managed a
  create <...&> destroy = managed (bracket create destroy)

  eventIsQPress = SDL.eventPayload >>> \case
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
        SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
    _ -> False
