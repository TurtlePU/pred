module Pred.MVU where

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
