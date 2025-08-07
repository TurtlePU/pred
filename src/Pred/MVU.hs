{- HLINT ignore "Use const" -}

module Pred.MVU where

data Destination m = forall t. To (m t)
data Destination' m = forall t. To' (m t) | Exit
type Update m e d = forall s. e s -> m s -> d m
type View m e v = forall s. m s -> v (e s)

runMVU :: Monad v => m s -> Update m e Destination -> View m e v -> v r
runMVU = runSystem \(To m) go -> go m

runMVU' :: Monad v => v r -> m s -> Update m e Destination' -> View m e v -> v r
runMVU' final = runSystem \case
  To' m -> \go -> go m
  Exit -> \_ -> final

runSystem ::
  forall d v m s e r. Monad v =>
  (d m -> (forall t. m t -> v r) -> v r) ->
  m s -> Update m e d -> View m e v -> v r
runSystem onNewState initial update view = go initial
 where
  go :: m t -> v r
  go state = do
    event <- view state
    onNewState (update event state) go
