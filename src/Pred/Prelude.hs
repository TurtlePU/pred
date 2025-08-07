{-# LANGUAGE AllowAmbiguousTypes #-}

module Pred.Prelude
  ( module Data.Function
  , module Data.Kind
  , module Data.String
  , module GHC.Generics
  , module Prelude
  , module Pred.Prelude
  ) where

import Data.Function (fix)
import Data.Generics.Product qualified as Optics
import Data.Kind (Type)
import Data.String (fromString)
import GHC.Generics (Generic)
import Prelude

ifThenElse :: Bool -> p -> p -> p
ifThenElse b t e = case b of
  True  -> t
  False -> e

getField :: forall x r a. Optics.HasField' x r a => r -> a
getField = Optics.getField @x

setField :: forall x r a. Optics.HasField' x r a => r -> a -> r
setField = flip (Optics.setField @x)
