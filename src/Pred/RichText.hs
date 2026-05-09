module Pred.RichText
  ( SourceText
  , sourceText
  , VPC (..)
  , boundingBox
  , clampToBox
  , moveViewPort
  , TextViewPort (..)
  , pxToViewPort
  , blitTextViewPort
  ) where

import Control.Monad (when)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Ord (clamp)
import Foreign.C (CInt)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import SDL qualified

import Pred.TTF qualified as TTF

newtype SourceText = ST { stLines :: [(Int, Text)] }

sourceText :: Text -> SourceText
sourceText = ST . filter (not . Text.null . snd) . zip [0..] . Text.lines

(!) :: SourceText -> Int -> Text
ST st ! i = fromMaybe Text.empty (lookup i st)

-- | 'VPC' is short for "viewport coordinates".
data VPC a = VPC { column :: a, line :: a } deriving Functor

boundingBox :: SourceText -> VPC Int
boundingBox (ST st) =
  maximum . (0 :) <$> liftA2 VPC (Text.length . snd <$>) (fst <$>) st

clampToBox :: SourceText -> SDL.Point VPC Int -> SDL.Point VPC Int
clampToBox (boundingBox -> VPC maxC maxL) (SDL.P (VPC c l)) =
  SDL.P $ clamp (0, maxC) c `VPC` clamp (0, maxL) l

moveViewPort :: SourceText -> VPC Int -> SDL.Point VPC Int -> SDL.Point VPC Int
moveViewPort st (VPC dc dl) (SDL.P (VPC c l)) = SDL.P $ c' `VPC` l'
  where
    VPC _ maxL = boundingBox st
    l' = if dl == 0 then l else clamp (0, maxL) (l + dl)
    maxC = Text.length (st ! l')
    c' = if dc == 0 then c else clamp (0, maxC) (c + dc)

(!?) :: SourceText -> SDL.Point VPC Int -> Maybe Char
ST st !? SDL.P vpc = lookup (line vpc) st >>= safeIndex (column vpc)
  where
    safeIndex :: Int -> Text -> Maybe Char
    safeIndex i t
      | i < Text.length t = Just $ Text.index t i
      | otherwise = Nothing

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
  fc <- TTF.load fonts (font tvp)
  lineSkip <- TTF.lineSkip fc
  let SDL.P pos = position tvp
      line' = line pos + y `div` lineSkip
      lineText = source tvp ! line'
  column <- binarySearch (-1, Text.length lineText) \i -> do
    (wl, _) <- TTF.size fc $ Text.drop (column pos) $ Text.take (max 0 i) lineText
    (wr, _) <- TTF.size fc $ Text.drop (column pos) $ Text.take (i + 1) lineText
    pure $ (wl + wr) `div` 2 > x
  line <- pure line'
  pure $ SDL.P (VPC column line)
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
  SDL.surfaceFillRect surface Nothing (bgColor tvp)
  let SDL.P vec = position tvp
  fc <- TTF.load fonts (font tvp)
  lineSkip <- TTF.lineSkip fc
  colSkip <- do
    let pos = fromIntegral (column vec)
        start = Text.take pos $ source tvp ! line vec
    (trueWidth, _) <- TTF.size fc start
    charWidth <- advance fc 'o'
    pure $ trueWidth + charWidth * max 0 (pos - Text.length start)
  bounds <- SDL.surfaceDimensions surface
  for_ (stLines $ source tvp) \(i, line') -> do
    let blitY = toEnum $ (i - line vec) * lineSkip
        blitPos = SDL.P $ SDL.V2 (-toEnum colSkip) blitY
        SDL.V2 _ maxY = bounds
    when (0 <= blitY && blitY < maxY) do
      lineSurface <- TTF.solid fc (textColor tvp) line'
      _ <- SDL.surfaceBlit lineSurface Nothing surface $ Just blitPos
      pure ()
  for_ (selection tvp) \selectionPos -> do
    cPX <- viewPortToPx fc selectionPos
    when (cPX `inBounds` bounds) do
      let char = fromMaybe ' ' $ source tvp !? selectionPos
      charWidth <- advance fc char
      let rect = SDL.Rectangle cPX (toEnum <$> SDL.V2 charWidth lineSkip)
      SDL.surfaceFillRect surface (Just rect) (textColor tvp)
      charSurface <- TTF.solid fc (bgColor tvp) (Text.singleton char)
      _ <- SDL.surfaceBlit charSurface Nothing surface $ Just cPX
      pure ()
  for_ (cursors tvp) \cursorPos -> do
    cPX <- viewPortToPx fc cursorPos
    when (cPX `inBounds` bounds) do
      let char = fromMaybe ' ' $ source tvp !? cursorPos
      charWidth <- advance fc char
      let rect = SDL.Rectangle cPX
            (toEnum <$> SDL.V2 (charWidth `div` 5) lineSkip)
      SDL.surfaceFillRect surface (Just rect) (textColor tvp)
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
      let SDL.P pos = position tvp
          lineText = source tvp ! line vpc
      (columnPx, _) <-
        if column vpc < column pos
        then pure (-1, -1)
        else TTF.size fc $ Text.drop (column pos)
                         $ Text.take (column vpc) lineText
      lineSkip <- TTF.lineSkip fc
      pure $ SDL.P $ toEnum <$> columnPx `SDL.V2`
        ((line vpc - line pos) * lineSkip)
