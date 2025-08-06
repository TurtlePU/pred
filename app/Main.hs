module Main (main) where

import Control.Category ((>>>))
import Control.Exception (bracket)
import Control.Monad.Managed (liftIO, managed, runManaged)
import Data.Kind (Type)
import SDL qualified
import System.Exit (exitSuccess)

type Destination m = forall r. (forall t. m t -> r) -> r

to :: m t -> a -> Destination m
to m = const $ here m

here :: m t -> Destination m
here m k = k m

type Update m e = forall s. e s -> m s -> Destination m
type View m e v = forall s. m s -> v (e s)

runMVU ::
  forall m e v s r. Monad v => m s -> Update m e -> View m e v -> v r
runMVU initial update view = go initial
 where
  go :: m t -> v r
  go state = do
    event <- view state
    update event state go

data State = Init | Idle

data Model :: State -> Type where
  MInit :: Model Init
  MIdle :: SDL.Renderer -> Model Idle

data Event :: State -> Type where
  Ready :: SDL.Renderer -> Event Init
  Stay :: Event Idle

main :: IO ()
main = runManaged $ runMVU MInit routeTable \case
 MInit -> do
  SDL.initializeAll
  window <- managed $ flip bracket SDL.destroyWindow $
    SDL.createWindow "PrEd proof editor" SDL.defaultWindow
      { SDL.windowHighDPI = True
      , SDL.windowMode = SDL.Maximized
      , SDL.windowResizable = True
      }
  Ready <$> SDL.createRenderer window (-1) SDL.defaultRenderer
 MIdle renderer -> do
  events <- SDL.pollEvents
  let qPressed = any eventIsQPress events
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
  SDL.clear renderer
  SDL.present renderer
  if qPressed then liftIO exitSuccess else pure Stay
 where
  routeTable :: Event s -> Model s -> Destination Model
  routeTable = \case
    Ready renderer -> to (MIdle renderer)
    Stay -> here

  eventIsQPress = SDL.eventPayload >>> \case
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
        SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
    _ -> False
