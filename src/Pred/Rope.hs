module Pred.Rope where

import Data.Bifunctor (first)

import Control.Monad.State (get, modify, runState, state)

import Pred.Prelude

lscan :: Traversable f => (b -> a -> b) -> b -> f a -> (f b, b)
lscan next seed coll =
  traverse (\el -> modify (`next` el) >> get) coll `runState` seed

data Rope f a = Rope { delimited :: f a, end :: a }
  deriving (Foldable, Functor, Generic, Traversable)

delimit ::
  Traversable f =>
  (a -> i -> (a, a)) -> (p -> p -> i) -> p -> a -> f p -> Rope f a
delimit split diff z seed delims =
  let (fmap snd -> lens, _) =
        lscan (\(p, _) c -> (c, diff c p)) (z, diff z z) delims
      (delimited, end) = traverse (state . flip split) lens `runState` seed
   in Rope {..}

cumsum :: (Traversable f, Monoid a) => Rope f a -> (f a, a)
cumsum = first delimited . lscan (<>) mempty

onDelimited :: Functor f => (a -> a) -> Rope f a -> Rope f a
act `onDelimited` rope = rope { delimited = act <$> rope.delimited }
