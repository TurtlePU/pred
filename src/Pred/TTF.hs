module Pred.TTF
  ( Fonts
  , newFonts
  , closeFonts
  , Font (..)
  , editorFont
  , FontCache
  , glyphMetrics
  , lineSkip
  , size
  , solid
  ) where

import Data.IORef (IORef)
import Data.IORef qualified as IORef
import System.Mem.Weak (Weak)
import System.Mem.Weak qualified as Weak

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import SDL qualified
import SDL.Font qualified as TTF

import Pred.Prelude

newtype Fonts = MkFonts { palette :: IORef Palette }

newtype Palette = MkPalette { editorFC :: Maybe (Font, Weak FontCache) }

newFonts :: IO Fonts
newFonts = TTF.initialize >> MkFonts <$> IORef.newIORef (MkPalette Nothing)

closeFonts :: Fonts -> IO ()
closeFonts _ = TTF.quit

data Font = MkFont
  { path :: Text
  , pointSize :: TTF.PointSize
  }
  deriving (Generic, Eq, Show)

editorFont :: Fonts -> Font -> IO FontCache
editorFont MkFonts {..} font = do
  MkPalette {..} <- IORef.readIORef palette
  try <- case editorFC of
    Just (savedFont, fc) | font == savedFont -> Weak.deRefWeak fc
    _                                        -> pure Nothing
  case try of
    Just fc -> pure fc
    Nothing -> do
      fc <- newFontCache font
      fcw <- Weak.mkWeakPtr fc $ Just do
        IORef.writeIORef palette (MkPalette Nothing)
        TTF.free fc.font
      IORef.writeIORef palette MkPalette { editorFC = Just (font, fcw) }
      pure fc

data FontCache = MkFontCache
  { font     :: TTF.Font
  , surfaces :: IORef (Map (TTF.Color, Text) (Weak SDL.Surface))
  }
  deriving Generic

newFontCache :: Font -> IO FontCache
newFontCache MkFont {..} = MkFontCache
  <$> TTF.load (Text.unpack path) pointSize
  <*> IORef.newIORef Map.empty

glyphMetrics :: FontCache -> Char -> IO (Maybe (Int, Int, Int, Int, Int))
glyphMetrics MkFontCache {..} = TTF.glyphMetrics font

lineSkip :: FontCache -> IO Int
lineSkip MkFontCache {..} = TTF.lineSkip font

size :: FontCache -> Text -> IO (Int, Int)
size MkFontCache {..} = TTF.size font

solid :: FontCache -> TTF.Color -> Text -> IO SDL.Surface
solid MkFontCache {..} color text = do
  surfs <- IORef.readIORef surfaces
  let key = (color, text)
  try <- case surfs Map.!? key of
    Just ref -> Weak.deRefWeak ref
    Nothing -> pure Nothing
  case try of
    Just surface -> pure surface
    Nothing -> do
      surface <- TTF.solid font color text
      sw <- Weak.mkWeakPtr surface $ Just do
         IORef.modifyIORef surfaces (Map.delete key)
         SDL.freeSurface surface
      IORef.modifyIORef surfaces (Map.insert key sw)
      pure surface
