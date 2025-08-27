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

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Text qualified as Text
import SDL qualified
import SDL.Font qualified as TTF

import Pred.Prelude
import Pred.StableCache

newtype Fonts = MkFonts { cache :: StableCache Font FontCache }
  deriving Generic

newFonts :: IO Fonts
newFonts = TTF.initialize >> MkFonts <$> newStableCache

closeFonts :: Fonts -> IO ()
closeFonts fonts = closeCache fonts.cache >> TTF.quit

data Font = MkFont
  { path      :: Text
  , pointSize :: TTF.PointSize
  }
  deriving (Generic, Eq, Show)

load :: Fonts -> Font -> IO FontCache
load fonts font = request fonts.cache font do
  fontCache <- newFontCache font
  pure (fontCache, closeFontCache fontCache)

data FontCache = MkFontCache
  { font     :: TTF.Font
  , surfaces :: StableCache Color (StableCache Text SDL.Surface)
  }
  deriving Generic

newFontCache :: Font -> IO FontCache
newFontCache MkFont {..} = MkFontCache
  <$> TTF.load (Text.unpack path) pointSize
  <*> newStableCache

closeFontCache :: FontCache -> IO ()
closeFontCache fc = closeCache fc.surfaces >> TTF.free fc.font

glyphMetrics ::
  MonadIO m => FontCache -> Char -> m (Maybe (Int, Int, Int, Int, Int))
glyphMetrics fc = TTF.glyphMetrics fc.font

lineSkip :: MonadIO m => FontCache -> m Int
lineSkip fc = TTF.lineSkip fc.font

size :: MonadIO m => FontCache -> Text -> m (Int, Int)
size fc = TTF.size fc.font

type Color = TTF.Color

solid :: MonadIO m => FontCache -> Color -> Text -> m SDL.Surface
solid fc color text = do
  perColor <- request fc.surfaces color do
    cache <- newStableCache
    pure (cache, closeCache cache)
  request perColor text do
    surface <- TTF.solid fc.font color text
    pure (surface, SDL.freeSurface surface)
