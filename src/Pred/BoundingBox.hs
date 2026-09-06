module Pred.BoundingBox where

import Data.Ord (clamp)

import SDL qualified

data BoundingBox f a = BB { bbStart :: f a, bbEnd :: f a }

clampToBox ::
  (Applicative f, Ord a) =>
  BoundingBox f a -> SDL.Point f a -> SDL.Point f a
clampToBox (BB st en) (SDL.P xx) = SDL.P (curry clamp <$> st <*> en <*> xx)
