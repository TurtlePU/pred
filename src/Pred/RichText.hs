module Pred.RichText
  ( TextViewPort (..)
  , pxToViewPort
  , blitTextViewPort
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Foreign.C (CInt)
import Prelude hiding (splitAt)

import Data.IntMap qualified as IntMap
import Data.Text qualified as Text
import SDL qualified

import Pred.TTF qualified as TTF
import Pred.SourceText (SourceText)
import Pred.SourceText qualified as ST

data TextViewPort = TextViewPort
  { source    :: SourceText
  , font      :: TTF.Font
  , bgColor   :: TTF.Color
  , textColor :: TTF.Color
  , position  :: SDL.Point ST.VPC Int
  , selection :: [SDL.Point ST.VPC Int]
  , cursors   :: [SDL.Point ST.VPC Int]
  }

pxToViewPort ::
  MonadIO m =>
  TextViewPort -> TTF.Fonts -> SDL.Point SDL.V2 Int -> m (SDL.Point ST.VPC Int)
pxToViewPort tvp fonts (SDL.P (SDL.V2 x y)) = do
  fc <- TTF.load fonts tvp.font
  lineSkip <- TTF.lineSkip fc
  let SDL.P pos = tvp.position
      line = pos.line + y `div` lineSkip
      lineText = tvp.source ST.! line
  column <- binarySearch (-1, Text.length lineText) \i -> do
    (wl, _) <- TTF.size fc $ Text.drop pos.column $ Text.take (max 0 i) lineText
    (wr, _) <- TTF.size fc $ Text.drop pos.column $ Text.take (i + 1) lineText
    pure $ (wl + wr) `div` 2 > x
  pure $ SDL.P ST.VPC { column = column, line = line }
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
        start = Text.take pos (tvp.source ST.! vec.line)
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
      let char = fromMaybe ' ' (tvp.source ST.!? selectionPos)
      charWidth <- advance fc char
      let rect = SDL.Rectangle cPX (toEnum <$> SDL.V2 charWidth lineSkip)
      SDL.surfaceFillRect surface (Just rect) tvp.textColor
      charSurface <- TTF.solid fc tvp.bgColor (Text.singleton char)
      _ <- SDL.surfaceBlit charSurface Nothing surface (Just cPX)
      pure ()
  for_ tvp.cursors \cursorPos -> do
    cPX <- viewPortToPx fc cursorPos
    when (cPX `inBounds` bounds) do
      let char = fromMaybe ' ' (tvp.source ST.!? cursorPos)
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
      MonadIO m => TTF.FontCache -> SDL.Point ST.VPC Int ->
      m (SDL.Point SDL.V2 CInt)
    viewPortToPx fc (SDL.P vpc) = do
      let SDL.P pos = tvp.position
          lineText = tvp.source ST.! vpc.line
      (columnPx, _) <-
        if vpc.column < pos.column
        then pure (-1, -1)
        else TTF.size fc $ Text.drop pos.column
                         $ Text.take vpc.column lineText
      lineSkip <- TTF.lineSkip fc
      pure $ SDL.P $ toEnum <$> columnPx `SDL.V2`
        ((vpc.line - pos.line) * lineSkip)
