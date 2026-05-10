module Pred.SourceText
  ( SourceText (stLines)
  , sourceText
  , (!)
  , (<<>>)
  , VPC (..)
  , clampToBox
  , moveViewPort
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
import Prelude hiding (splitAt)

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.MultiSet (MultiSet)
import Data.Text (Text)
import Data.Text qualified as Text
import SDL qualified

data SourceText = ST
  { stLines :: IntMap Text
  , stLineCount :: Int
  }

sourceText :: Text -> SourceText
sourceText (zip [0..] . Text.lines -> annot) = ST
  { stLines = IntMap.fromList $ filter (not . Text.null . snd) annot
  , stLineCount = maybe 0 (succ . fst . snd) (List.unsnoc annot)
  }

(!) :: SourceText -> Int -> Text
st ! i = fromMaybe Text.empty (st.stLines IntMap.!? i)

infixl 6 <<>>

(<<>>) :: SourceText -> SourceText -> SourceText
st <<>> st' = ST
    { stLines = IntMap.unionWith (<>) st.stLines (modifier st'.stLines)
    , stLineCount = st'.stLineCount + addend
    }
    where
      addend = if st.stLineCount == 0 then 0 else st.stLineCount - 1
      modifier = if addend == 0 then id else IntMap.mapKeys (+ addend)

instance Semigroup SourceText where
  (<>) = (<<>>)
  sconcat = foldl1' (<<>>)

instance Monoid SourceText where
  mempty = ST IntMap.empty 0
  mconcat = foldl' (<<>>) mempty

-- | 'VPC' is short for "viewport coordinates".
data VPC a = VPC { column :: a, line :: a } deriving (Eq, Functor)

instance Ord a => Ord (VPC a) where
    VPC c l `compare` VPC c' l' = compare l l' <> compare c c'

boundingBox :: SourceText -> VPC Int
boundingBox st = VPC
  { column = maximum $ 0 : [ Text.length l | l <- IntMap.elems st.stLines ]
  , line = st.stLineCount
  }

clampToBox :: SourceText -> SDL.Point VPC Int -> SDL.Point VPC Int
clampToBox (boundingBox -> VPC { column = maxC, line = maxL})
    (SDL.P VPC { column = c, line = l }) =
  SDL.P VPC { column = clamp (0, maxC) c, line = clamp (0, maxL) l }

moveViewPort :: SourceText -> VPC Int -> SDL.Point VPC Int -> SDL.Point VPC Int
moveViewPort st VPC { column = dc, line = dl }
    (SDL.P VPC { column = c, line = l }) =
  SDL.P VPC { column = c', line = l' }
  where
    VPC { line = maxL } = boundingBox st
    l' = if dl == 0 then l else clamp (0, maxL) (l + dl)
    maxC = Text.length (st ! l')
    c' = if dc == 0 then c else clamp (0, maxC) (c + dc)

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
        { stLines = IntMap.filterKeys (> line) st.stLines
                 <> IntMap.fromList [ (line, end) | not (Text.null end) ]
        , stLineCount = 0 `max` st.stLineCount - line
        }
      )

splits :: MultiSet (SDL.Point VPC Int) -> SourceText -> NonEmpty SourceText
splits is st =
  let scan = NonEmpty.scanr (\p -> splitAt p . fst) (st, st) is
   in fst (NonEmpty.head scan) NonEmpty.:| fmap snd (NonEmpty.init scan)

insert :: SDL.Point VPC Int -> Text -> SourceText -> SourceText
insert i text (splitAt i -> (before, after)) =
  before <<>> sourceText text <<>> after

delete :: SDL.Point VPC Int -> VPC Int -> SourceText -> SourceText
delete pos len st =
    let pos' = moveViewPort st len pos
        (p, q) = (min pos pos', max pos pos')
     in let (splitAt p -> (st1, _), st3) = splitAt q st
        in st1 <<>> st3
