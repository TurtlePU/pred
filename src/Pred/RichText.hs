module Pred.RichText
  ( SourceText
  , sourceText
  , VPC (..)
  , boundingBox
  , (!?)
  , TextViewPort (..)
  , blitVisibleText
  , blitSelection
  , blitCursor
  , viewPortToPx
  , pxToViewPort
  ) where

import Control.Monad (when)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import SDL qualified

import Pred.Prelude
import Pred.TTF qualified as TTF

newtype SourceText = ST { stLines :: [(Int, Text)] } deriving Generic

sourceText :: Text -> SourceText
sourceText = ST . filter (not . Text.null . snd) . zip [0..] . Text.lines

(!) :: SourceText -> Int -> Text
ST st ! i = fromMaybe "" (lookup i st)

-- | 'VPC' is short for "viewport coordinates".
data VPC a = VPC { column :: a, line :: a } deriving (Functor, Generic)

boundingBox :: SourceText -> VPC Int
boundingBox (ST st) =
  maximum . (0 :) <$> liftA2 VPC (Text.length . snd <$>) (fst <$>) st

(!?) :: SourceText -> SDL.Point VPC Int -> Maybe Char
ST st !? SDL.P vpc = lookup vpc.line st >>= safeIndex vpc.column
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
  } deriving Generic

blitVisibleText ::
  MonadIO m => SDL.Surface -> TTF.Fonts -> TextViewPort -> m ()
blitVisibleText surface fonts tvp = do
  SDL.surfaceFillRect surface Nothing tvp.bgColor
  let SDL.P vec = tvp.position
  fc <- TTF.load fonts tvp.font
  lineSkip <- toEnum <$> TTF.lineSkip fc
  colSkip <- do
    let pos = fromIntegral vec.column
        start = Text.take pos $ tvp.source ! vec.line
    (trueWidth, _) <- TTF.size fc start
    charWidth <- advance fc 'o'
    pure $ trueWidth + charWidth * max 0 (pos - Text.length start)
  SDL.V2 _ surfaceHeight <- SDL.surfaceDimensions surface
  for_ tvp.source.stLines \(i, line) -> do
    let blitY = toEnum (i - vec.line) * lineSkip
        blitPos = SDL.V2 (-toEnum colSkip) blitY
    if 0 <= blitY && blitY < surfaceHeight
    then do
      lineSurface <- TTF.solid fc tvp.textColor line
      SDL.surfaceBlit lineSurface Nothing surface $ Just (SDL.P blitPos)
    else pure Nothing

blitSelection ::
  MonadIO m =>
  SDL.Surface -> TTF.Fonts -> TextViewPort -> SDL.Point VPC Int -> m ()
blitSelection surface fonts tvp selectionPos = do
  cPX <- viewPortToPx tvp fonts selectionPos
  bounds <- fmap fromEnum <$> SDL.surfaceDimensions surface
  when (cPX `inBounds` bounds) do
    let char = fromMaybe ' ' $ tvp.source !? selectionPos
    fc <- TTF.load fonts tvp.font
    charWidth <- advance fc char
    lineSkip <- TTF.lineSkip fc
    let rect = toEnum <$> SDL.Rectangle cPX (charWidth `SDL.V2` lineSkip)
    SDL.surfaceFillRect surface (Just rect) tvp.textColor
    charSurface <- TTF.solid fc tvp.bgColor (Text.singleton char)
    _ <- SDL.surfaceBlit charSurface Nothing surface $ Just $ toEnum <$> cPX
    pure ()

blitCursor ::
  MonadIO m =>
  SDL.Surface -> TTF.Fonts -> TextViewPort -> SDL.Point VPC Int -> m ()
blitCursor surface fonts tvp cursorPos = do
  cPX <- viewPortToPx tvp fonts cursorPos
  bounds <- fmap fromEnum <$> SDL.surfaceDimensions surface
  when (cPX `inBounds` bounds) do
    let char = fromMaybe ' ' $ tvp.source !? cursorPos
    fc <- TTF.load fonts tvp.font
    charWidth <- advance fc char
    lineSkip <- TTF.lineSkip fc
    let rect = toEnum <$>
          SDL.Rectangle cPX (SDL.V2 (charWidth `div` 5) lineSkip)
    SDL.surfaceFillRect surface (Just rect) tvp.textColor

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
  MonadIO m =>
  TextViewPort -> TTF.Fonts -> SDL.Point VPC Int -> m (SDL.Point SDL.V2 Int)
viewPortToPx tvp fonts (SDL.P vpc) = do
  let SDL.P pos = tvp.position
      lineText = tvp.source ! vpc.line
  fc <- TTF.load fonts tvp.font
  (columnPx, _) <- TTF.size fc $ Text.drop pos.column
                               $ Text.take vpc.column lineText
  lineSkip <- TTF.lineSkip fc
  pure $ SDL.P $ columnPx `SDL.V2` ((vpc.line - pos.line) * lineSkip)

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
  pure $ SDL.P VPC {..}

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
