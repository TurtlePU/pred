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
  , blended
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

load :: MonadIO m => Fonts -> Font -> m FontCache
load fonts font = request fonts.cache font do
  fontCache <- newFontCache font
  pure (fontCache, closeFontCache fontCache)

data FontCache = MkFontCache
  { font     :: TTF.Font
  , surfaces :: StableCache (BlitMode, Color) (StableCache Text SDL.Surface)
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

data BlitMode
  = Solid
  | Blended
  deriving (Eq, Ord, Show)

renderTextSurface
  :: MonadIO m
  => FontCache -> BlitMode -> Color -> Text -> m SDL.Surface
renderTextSurface fc mode fg text = do
  perKey <- request fc.surfaces (mode, fg) do
    cache <- newStableCache
    pure (cache, closeCache cache)

  request perKey text do
    surface <- case mode of
      Solid        -> TTF.solid   fc.font fg text
      Blended      -> TTF.blended fc.font fg text
    pure (surface, SDL.freeSurface surface)



solid :: MonadIO m => FontCache -> Color -> Text -> m SDL.Surface
solid fc fg txt = renderTextSurface fc Solid fg txt

blended :: MonadIO m => FontCache -> Color -> Text -> m SDL.Surface
blended fc fg txt = renderTextSurface fc Blended fg txt
