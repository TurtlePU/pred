module Pred.TTF
  ( Fonts
  , newFonts
  , closeFonts
  , Font (..)
  , load
  , FontCache
  , glyphMetrics
  , lineSkip
  , size
  , Color
  , solid
  ) where

import GHC.Generics (Generic)

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Text qualified as Text
import SDL qualified
import SDL.Font qualified as TTF

import Pred.StableCache

newtype Fonts = MkFonts { cache :: StableCache Font FontCache }

newFonts :: IO Fonts
newFonts = TTF.initialize >> MkFonts <$> newStableCache

closeFonts :: Fonts -> IO ()
closeFonts fonts = closeCache (cache fonts) >> TTF.quit

data Font = MkFont
  { path      :: Text
  , pointSize :: TTF.PointSize
  }
  deriving (Eq, Generic, Show)

load :: MonadIO m => Fonts -> Font -> m FontCache
load fonts font = request (cache fonts) font do
  fontCache <- newFontCache font
  pure (fontCache, closeFontCache fontCache)

data FontCache =
  MkFontCache TTF.Font (StableCache Color (StableCache Text SDL.Surface))

fcFont :: FontCache -> TTF.Font
fcFont (MkFontCache font _) = font

newFontCache :: Font -> IO FontCache
newFontCache (MkFont path pointSize) = MkFontCache
  <$> TTF.load (Text.unpack path) pointSize
  <*> newStableCache

closeFontCache :: FontCache -> IO ()
closeFontCache (MkFontCache font surfaces) =
  closeCache surfaces >> TTF.free font

glyphMetrics ::
  MonadIO m => FontCache -> Char -> m (Maybe (Int, Int, Int, Int, Int))
glyphMetrics = TTF.glyphMetrics . fcFont

lineSkip :: MonadIO m => FontCache -> m Int
lineSkip = TTF.lineSkip . fcFont

size :: MonadIO m => FontCache -> Text -> m (Int, Int)
size = TTF.size . fcFont

type Color = TTF.Color

solid :: MonadIO m => FontCache -> Color -> Text -> m SDL.Surface
solid (MkFontCache font surfaces) color text = do
  perColor <- request surfaces color do
    cache <- newStableCache
    pure (cache, closeCache cache)
  request perColor text do
    surface <- TTF.solid font color text
    pure (surface, SDL.freeSurface surface)
