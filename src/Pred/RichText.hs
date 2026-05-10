module Pred.RichText
  ( SourceText
  , sourceText
  , VPC (..)
  , boundingBox
  , clampToBox
  , moveViewPort
  , splitAt
  , insert
  , TextViewPort (..)
  , pxToViewPort
  , blitTextViewPort
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.Foldable1 (foldl1')
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Ord (clamp)
import Data.Semigroup (Semigroup (..))
import Foreign.C (CInt)
import Prelude hiding (splitAt)

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as Text
import SDL qualified

import Pred.TTF qualified as TTF

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
data VPC a = VPC { column :: a, line :: a } deriving Functor

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

insert :: SDL.Point VPC Int -> Text -> SourceText -> SourceText
insert i text (splitAt i -> (before, after)) =
  before <<>> sourceText text <<>> after

data TextViewPort = TextViewPort
  { source    :: SourceText
  , font      :: TTF.Font
  , bgColor   :: TTF.Color
  , textColor :: TTF.Color
  , position  :: SDL.Point VPC Int
  , selection :: [SDL.Point VPC Int]
  , cursors   :: [SDL.Point VPC Int]
  }

pxToViewPort ::
  MonadIO m =>
  TextViewPort -> TTF.Fonts -> SDL.Point SDL.V2 Int -> m (SDL.Point VPC Int)
pxToViewPort tvp fonts (SDL.P (SDL.V2 x y)) = do
  fc <- TTF.load fonts tvp.font
  lineSkip <- TTF.lineSkip fc
  let SDL.P pos = tvp.position
      line = pos.line + y `div` lineSkip
      lineText = tvp.source ! line
  column <- binarySearch (-1, Text.length lineText) \i -> do
    (wl, _) <- TTF.size fc $ Text.drop pos.column $ Text.take (max 0 i) lineText
    (wr, _) <- TTF.size fc $ Text.drop pos.column $ Text.take (i + 1) lineText
    pure $ (wl + wr) `div` 2 > x
  pure $ SDL.P VPC { column = column, line = line }
  where
    binarySearch :: (Integral a, Monad m) => (a, a) -> (a -> m Bool) -> m a
    binarySearch (start, end) test = go start end
      where
        go lo hi
          | succ lo >= hi = pure hi
          | otherwise = do
             let mid = lo + (hi - lo) `div` 2
             test mid >>= \case
               True -> go lo mid
               False -> go mid hi

blitTextViewPort ::
  MonadIO m => SDL.Surface -> TTF.Fonts -> TextViewPort -> m ()
blitTextViewPort surface fonts tvp = do
  SDL.surfaceFillRect surface Nothing tvp.bgColor
  let SDL.P vec = tvp.position
  fc <- TTF.load fonts tvp.font
  lineSkip <- TTF.lineSkip fc
  colSkip <- do
    let pos = fromIntegral vec.column
        start = Text.take pos (tvp.source ! vec.line)
    (trueWidth, _) <- TTF.size fc start
    charWidth <- advance fc 'o'
    pure $ trueWidth + charWidth * max 0 (pos - Text.length start)
  bounds <- SDL.surfaceDimensions surface
  for_ (IntMap.assocs tvp.source.stLines) \(i, line) -> do
    let blitY = toEnum $ (i - vec.line) * lineSkip
        blitPos = SDL.P $ SDL.V2 (-toEnum colSkip) blitY
        SDL.V2 _ maxY = bounds
    when (0 <= blitY && blitY < maxY) do
      lineSurface <- TTF.solid fc tvp.textColor line
      _ <- SDL.surfaceBlit lineSurface Nothing surface (Just blitPos)
      pure ()
  for_ tvp.selection \selectionPos -> do
    cPX <- viewPortToPx fc selectionPos
    when (cPX `inBounds` bounds) do
      let char = fromMaybe ' ' (tvp.source !? selectionPos)
      charWidth <- advance fc char
      let rect = SDL.Rectangle cPX (toEnum <$> SDL.V2 charWidth lineSkip)
      SDL.surfaceFillRect surface (Just rect) tvp.textColor
      charSurface <- TTF.solid fc tvp.bgColor (Text.singleton char)
      _ <- SDL.surfaceBlit charSurface Nothing surface (Just cPX)
      pure ()
  for_ tvp.cursors \cursorPos -> do
    cPX <- viewPortToPx fc cursorPos
    when (cPX `inBounds` bounds) do
      let char = fromMaybe ' ' (tvp.source !? cursorPos)
      charWidth <- advance fc char
      let rect = SDL.Rectangle cPX
            (toEnum <$> SDL.V2 (charWidth `div` 5) lineSkip)
      SDL.surfaceFillRect surface (Just rect) tvp.textColor
  where
    advance :: MonadIO m => TTF.FontCache -> Char -> m Int
    advance fc char = liftIO do
      Just (_, _, _, _, adv) <- TTF.glyphMetrics fc char
      pure adv

    inBounds ::
      (Applicative f, Foldable f, Num a, Ord a) => SDL.Point f a -> f a -> Bool
    inBounds (SDL.P v) b = all (uncurry inBound) $ liftA2 (,) v b
      where
        inBound x maxX = 0 <= x && x <= maxX

    viewPortToPx ::
      MonadIO m => TTF.FontCache -> SDL.Point VPC Int ->
      m (SDL.Point SDL.V2 CInt)
    viewPortToPx fc (SDL.P vpc) = do
      let SDL.P pos = tvp.position
          lineText = tvp.source ! vpc.line
      (columnPx, _) <-
        if vpc.column < pos.column
        then pure (-1, -1)
        else TTF.size fc $ Text.drop pos.column
                         $ Text.take vpc.column lineText
      lineSkip <- TTF.lineSkip fc
      pure $ SDL.P $ toEnum <$> columnPx `SDL.V2`
        ((vpc.line - pos.line) * lineSkip)
