module Pred.BoundingBox where

import Data.Ord (clamp)

import SDL qualified

newtype BoundingBox f a = BB { boundingBox :: f a }

clampToBox ::
  (Applicative f, Num a, Ord a) =>
  BoundingBox f a -> SDL.Point f a -> SDL.Point f a
clampToBox (BB bb) (SDL.P xx) = SDL.P (clamper <$> bb <*> xx)
  where clamper b x = clamp (0, b) x
