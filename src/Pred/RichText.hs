{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Ord (clamp)

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Text qualified as Text
import SDL qualified

import Pred.Prelude
import Pred.TTF qualified as TTF
import Pred.Fonts (chooseFcFromList)

-- | Internally keep just the lines; index safely through helpers.
newtype SourceText = ST { unLines :: [Text] } deriving (Generic)

sourceText :: Text -> SourceText
sourceText = ST . Text.lines

(!) :: SourceText -> Int -> Text
ST ls ! i
  | i < 0 || i >= length ls = ""
  | otherwise               = ls !! i

-- | 'VPC' is short for "viewport coordinates".
data VPC a = VPC { column :: a, line :: a } deriving (Functor, Generic)

boundingBox :: SourceText -> VPC Int
boundingBox (ST ls) =
  let maxC = maximum (0 : map Text.length ls)
      maxL = max 0 (length ls - 1)
  in VPC maxC maxL

clampToBox :: SourceText -> SDL.Point VPC Int -> SDL.Point VPC Int
clampToBox (boundingBox -> VPC maxC maxL) (SDL.P (VPC c l)) =
  SDL.P $ VPC (clamp (0, maxC) c) (clamp (0, maxL) l)

moveViewPort :: SourceText -> VPC Int -> SDL.Point VPC Int -> SDL.Point VPC Int
moveViewPort st (VPC dc dl) (SDL.P (VPC c l)) = SDL.P $ VPC c' l'
  where
    VPC _ maxL = boundingBox st
    l'   = if dl == 0 then l else clamp (0, maxL) (l + dl)
    maxC = Text.length (st ! l')
    c'   = if dc == 0 then c else clamp (0, maxC) (c + dc)

(!?) :: SourceText -> SDL.Point VPC Int -> Maybe Char
ST ls !? SDL.P (VPC col ln)
  | ln < 0 || ln >= length ls = Nothing
  | otherwise =
      let t = ls !! ln in if col < Text.length t then Just (Text.index t col) else Nothing

-- | Everything the blitter needs each frame.
data TextViewPort = TextViewPort
  { source    :: SourceText
  , fontsPref :: NonEmpty TTF.Font -- | Config isn't required on each startup
  , bgColor   :: TTF.Color
  , textColor :: TTF.Color
  , position  :: SDL.Point VPC Int
  , selection :: [SDL.Point VPC Int]
  , cursors   :: [SDL.Point VPC Int]
  } deriving Generic

-- | Convert pixel position to (column,line) by measuring with the selected font(s).
pxToViewPort :: MonadIO m => TextViewPort -> TTF.Fonts -> SDL.Point SDL.V2 Int -> m (SDL.Point VPC Int)
pxToViewPort tvp fonts (SDL.P (SDL.V2 x y)) = do
  baseFc   <- TTF.load fonts (headNE tvp.fontsPref) -- | Using first font from preferences
  lineSkip <- TTF.lineSkip baseFc
  let SDL.P pos = tvp.position
      line      = pos.line + y `div` lineSkip
      lineText  = tvp.source ! line

  -- | Hopefully this works
  fc <- chooseFcFromList fonts tvp.fontsPref lineText

  let go (lo, hi)
        | succ lo >= hi = pure hi
        | otherwise = do
            let prefix n = Text.drop pos.column $ Text.take (max 0 n) lineText
                mid = lo + (hi - lo) `div` 2
            (wl, _) <- TTF.size fc (prefix mid)
            (wr, _) <- TTF.size fc (prefix (mid + 1))
            if (wl + wr) `div` 2 > x then go (lo, mid) else go (mid, hi)

  column <- go (-1, Text.length lineText)
  pure $ SDL.P VPC{ column, line }
  where
    headNE (f :| _) = f

-- | Draw the text viewport (monospace-first; fallbacks used only if needed).
blitTextViewPort :: MonadIO m => SDL.Surface -> TTF.Fonts -> TextViewPort -> m ()
blitTextViewPort surface fonts tvp = do
  -- | Fill background
  SDL.surfaceFillRect surface Nothing tvp.bgColor

  let SDL.P origin = tvp.position

  -- | Preferred font ftw
  baseFc   <- TTF.load fonts (headNE tvp.fontsPref)
  lineSkip <- TTF.lineSkip baseFc

  -- | Choose cache from our precomputed pref fonts list for a concrete text
  let chooseFc t = chooseFcFromList fonts tvp.fontsPref t

      -- | Convert (col,line) to pixel point relative to origin
      viewToPx (SDL.P (VPC col ln)) = do
        let t = tvp.source ! ln
        fc <- chooseFc t
        (px, _) <- TTF.size fc $ Text.drop origin.column $ Text.take col t
        pure $ SDL.P (fromIntegral <$> SDL.V2 px ((ln - origin.line) * lineSkip))

  bounds <- SDL.surfaceDimensions surface

  -- | Render visible lines
  let visible = zip [0..] (unLines tvp.source)
  for_ visible $ \(ln, txt) -> do
    let y = fromIntegral ((ln - origin.line) * lineSkip)
        SDL.V2 _ h = bounds
    when (y >= 0 && y < h && not (Text.null txt)) $ do
      fc <- chooseFc txt
      glyphSurf <- TTF.blended fc tvp.textColor txt
      -- | Left edge aligned with current viewport column
      (leftPx, _) <- TTF.size fc (Text.take origin.column txt)
      let dst' = SDL.P (SDL.V2 (fromIntegral (negate leftPx)) y)
      _ <- SDL.surfaceBlit glyphSurf Nothing surface (Just dst')
      pure ()

  -- | Selection highlight
  for_ tvp.selection $ \pos -> do
    p <- viewToPx pos
    let SDL.P v = pos
        lnTxt = tvp.source ! v.line
        ch    = fromMaybe ' ' (tvp.source !? pos)
    fc <- chooseFc lnTxt
    w  <- advance fc ch
    let r = SDL.Rectangle p (fromIntegral <$> SDL.V2 w lineSkip)
    SDL.surfaceFillRect surface (Just r) tvp.textColor

  -- | Blinking caret(s)
  for_ tvp.cursors $ \pos -> do
    p <- viewToPx pos
    let r = SDL.Rectangle p (fromIntegral <$> SDL.V2 (max 1 (lineSkip `div` 6)) lineSkip)
    SDL.surfaceFillRect surface (Just r) tvp.textColor
  where
    headNE (f :| _) = f
    advance fcache ch = do
      m <- TTF.glyphMetrics fcache ch
      pure $ maybe 0 (\(_,_,_,_,adv) -> adv) m
