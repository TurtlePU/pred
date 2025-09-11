{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Pred.Fonts
  ( buildMonospaceFallbacks   -- :: TTF.Font -> IO (NonEmpty TTF.Font)
  , chooseFcFromList          -- :: MonadIO m => TTF.Fonts -> NonEmpty TTF.Font -> Text -> m TTF.FontCache
  ) where

import Pred.Prelude

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Text.Font.Choose qualified as FC

import Pred.TTF qualified as TTF

-- | Some monospace fonts
monoCandidates :: [Text]
monoCandidates =
  [ "Noto Sans Mono", "DejaVu Sans Mono", "Consolas"
  , "Source Code Pro", "Courier New", "Menlo"
  ]

buildMonospaceFallbacks :: TTF.Font -> IO (NonEmpty TTF.Font)
buildMonospaceFallbacks base = do
  conf  <- FC.initLoadConfigAndFonts
  files <- forM monoCandidates $ \name ->
              pure (FC.fontMatch conf (FC.nameParse (Text.unpack name)) >>= FC.getValue "file")
  let mk p = base { path = Text.pack p }  -- | Keeps pointSize
  pure $ base :| map mk (catMaybes files)

chooseFcFromList :: MonadIO m => TTF.Fonts -> NonEmpty TTF.Font -> Text -> m TTF.FontCache
chooseFcFromList fonts (f0 :| fs) t = do
  -- | Scopes should be shared this way
  let try font = do
        cache <- TTF.load fonts font
        oks   <- mapM (TTF.glyphMetrics cache) (Text.unpack t)
        pure (if any (== Nothing) oks then Nothing else Just cache)

      go []     = TTF.load fonts f0
      go (f:xs) = do
        mc <- try f
        maybe (go xs) pure mc

  m0 <- try f0
  maybe (go fs) pure m0
