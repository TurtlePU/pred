module Pred.SourceText
  ( SourceText (stLines)
  , sourceText
  , toText
  , (!)
  , (<<>>)
  , VPC (..)
  , boundingBox
  , length
  , moveViewPort
  , clampToText
  , normalize
  , advance
  , (!?)
  , splitAt
  , splits
  , insert
  , delete
  ) where

import Data.Foldable1 (foldl1')
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Ord (clamp)
import Data.Semigroup (Semigroup (..))
import Data.String (fromString)
import Prelude hiding (length, splitAt)

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.MultiSet (MultiSet)
import Data.Text (Text)
import Data.Text qualified as Text
import SDL qualified

import Pred.BoundingBox (BoundingBox)
import Pred.BoundingBox qualified as BB

data SourceText = ST
  { stLines :: IntMap Text
  , stLineCount :: Int
  }

sourceText :: Text -> SourceText
sourceText (zip [0..] . Text.splitOn (fromString "\n") -> annot) = ST
  { stLines = IntMap.fromList $ filter (not . Text.null . snd) annot
  , stLineCount = List.length annot
  }

toText :: SourceText -> Text
toText = Text.intercalate (fromString "\n") . IntMap.elems . \st ->
  st.stLines <> IntMap.fromList (map (, Text.empty) [0 .. st.stLineCount - 1])

infix 5 !

(!) :: SourceText -> Int -> Text
st ! i = fromMaybe Text.empty (st.stLines IntMap.!? i)

infixl 6 <<>>

(<<>>) :: SourceText -> SourceText -> SourceText
st <<>> st' = ST
  { stLines = IntMap.unionWith (<>) st.stLines (modifier st'.stLines)
  , stLineCount = st'.stLineCount + addend
  }
  where
    addend = st.stLineCount - 1
    modifier = if addend == 0 then id else IntMap.mapKeys (+ addend)

instance Semigroup SourceText where
  (<>) = (<<>>)
  sconcat = foldl1' (<<>>)

instance Monoid SourceText where
  mempty = ST IntMap.empty 1
  mconcat = foldl' (<<>>) mempty

-- | 'VPC' is short for "viewport coordinates".
data VPC a = VPC { column :: a, line :: a } deriving (Eq, Functor)

instance (Eq a, Num a) => Semigroup (VPC a) where
  VPC c l <> VPC c' l'
    | l' == 0 = VPC (c + c') l
    | otherwise = VPC c' (l + l')

instance (Eq a, Num a) => Monoid (VPC a) where
  mempty = VPC 0 0

instance Ord a => Ord (VPC a) where
  VPC c l `compare` VPC c' l' = compare l l' <> compare c c'

instance Applicative VPC where
  pure x = VPC x x
  VPC cf lf <*> VPC cx lx = VPC (cf cx) (lf lx)

boundingBox :: SourceText -> BoundingBox VPC Int
boundingBox st = BB.BB VPC
  { column = maximum $ 0 : [ Text.length l | l <- IntMap.elems st.stLines ]
  , line = st.stLineCount
  }

length :: SourceText -> VPC Int
length st = VPC
  { column = Text.length (st ! st.stLineCount - 1)
  , line = st.stLineCount - 1
  }

moveViewPort :: SourceText -> VPC Int -> SDL.Point VPC Int -> SDL.Point VPC Int
moveViewPort st VPC { column = dc, line = dl }
    (SDL.P VPC { column = c, line = l }) =
  SDL.P VPC { column = c', line = l' }
  where
    BB.BB VPC { line = maxL } = boundingBox st
    l' = if dl == 0 then l else clamp (0, maxL) (l + dl)
    maxC = Text.length (st ! l')
    c' = if dc == 0 then c else clamp (0, maxC) (c + dc)

clampToText :: SourceText -> SDL.Point VPC Int -> SDL.Point VPC Int
clampToText st (SDL.P VPC { column, line }) =
  let line' = clamp (0, st.stLineCount - 1) line
   in SDL.P VPC
    { column = clamp (0, Text.length (st ! line')) column
    , line = line'
    }

normalize :: SourceText -> SDL.Point VPC Int -> SDL.Point VPC Int
normalize st p@(SDL.P VPC { column, line })
  | line < 0 = SDL.P (VPC 0 0)
  | column < 0 = normalize st $ SDL.P VPC
    { column = column + Text.length (st ! line - 1) + 1
    , line = line - 1
    }
  | line >= st.stLineCount = SDL.P (length st)
  | column > Text.length (st ! line) = normalize st $ SDL.P VPC
    { column = column - Text.length (st ! line) - 1
    , line = line + 1
    }
  | otherwise = p

advance :: Int -> SourceText -> SDL.Point VPC Int -> SDL.Point VPC Int
advance n st p = normalize st (p <> SDL.P VPC { column = n, line = 0 })

(!?) :: SourceText -> SDL.Point VPC Int -> Maybe Char
st !? SDL.P vpc = IntMap.lookup vpc.line st.stLines >>= safeIndex vpc.column
  where
    safeIndex :: Int -> Text -> Maybe Char
    safeIndex i t
      | i < Text.length t = Just $ Text.index t i
      | otherwise = Nothing

splitAt :: SDL.Point VPC Int -> SourceText -> (SourceText, SourceText)
splitAt (SDL.P VPC { line, column }) st =
  let (start, end) = Text.splitAt column (st ! line)
   in ( ST
        { stLines = IntMap.filterKeys (< line) st.stLines
                 <> IntMap.fromList [ (line, start) | not (Text.null start) ]
        , stLineCount = line + 1 `min` st.stLineCount
        }
      , ST
        { stLines = IntMap.mapKeys (subtract line)
                 $ IntMap.filterKeys (> line) st.stLines
                 <> IntMap.fromList [ (line, end) | not (Text.null end) ]
        , stLineCount = 1 `max` st.stLineCount - line
        }
      )

splits :: MultiSet (SDL.Point VPC Int) -> SourceText -> NonEmpty SourceText
splits is st =
  let scan = NonEmpty.scanr (\p -> splitAt p . fst) (st, st) is
   in fst (NonEmpty.head scan) NonEmpty.:| fmap snd (NonEmpty.init scan)

insert :: SDL.Point VPC Int -> Text -> SourceText -> SourceText
insert i text (splitAt i -> (before, after)) =
  before <<>> sourceText text <<>> after

delete :: (SDL.Point VPC Int, SDL.Point VPC Int) -> SourceText -> SourceText
delete (pos, pos') st =
  let (p, q) = (min pos pos', max pos pos')
   in let (splitAt p -> (st1, _), st3) = splitAt q st
      in st1 <<>> st3
