module Pred.MVU where

data Destination m r = forall t. To (m t) | Exit r
type Update m e r = forall s. e s -> m s -> Destination m r
type View m e v = forall s. m s -> v (e s)

runMVU :: forall v m s e r. Monad v => m s -> Update m e r -> View m e v -> v r
runMVU initial update view = go initial
 where
  go :: m t -> v r
  go state = do
    event <- view state
    case update event state of
      To newState -> go newState
      Exit result -> return result
