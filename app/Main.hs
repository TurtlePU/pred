module Main (main) where

import Control.Applicative ((<**>))
import Control.Monad (forever)
import Data.Foldable (for_)
import Data.Functor (($>), (<&>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Ord (clamp)
import Data.Traversable (for)
import Foreign.C (CInt)
import System.Exit (exitSuccess)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource qualified as Resource
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Graphics.Text.Font.Choose qualified as FC
import Options.Applicative qualified as Opt
import Reactive.Banana qualified as Banana
import Reactive.Banana.Frameworks qualified as Banana
import SDL qualified
import SDL.Font qualified as TTF
import System.Directory qualified as Dir
import Toml qualified

import Pred.Prelude

data Config = MkConfig
  { fontPath :: Text
  , fontSize :: TTF.PointSize
  }
  deriving (Generic, Eq)

data FontCache = MkFontCache
  { lastConfig   :: Config
  , lastFont     :: TTF.Font
  , lastSurfaces :: [(CInt, Text, SDL.Surface)]
  }
  deriving Generic

newFontCache :: MonadIO m => Config -> [(CInt, Text)] -> m FontCache
newFontCache lastConfig textLines = do
  lastFont <- TTF.load (Text.unpack lastConfig.fontPath) lastConfig.fontSize
  lastSurfaces <- for textLines \(i, l) ->
    (i,l,) <$> TTF.solid lastFont (SDL.V4 255 255 255 255) l
  pure MkFontCache {..}

freeCache :: MonadIO m => FontCache -> m ()
freeCache cache = do
  TTF.free cache.lastFont
  for_ cache.lastSurfaces \(_,_,s) -> SDL.freeSurface s

refreshCache :: MonadIO m => Config -> FontCache -> m FontCache
refreshCache newConfig cache =
  if newConfig == cache.lastConfig
  then pure cache
  else do
    freeCache cache
    newFontCache newConfig $ cache.lastSurfaces <&> \(i,l,_) -> (i,l)

refreshed :: MonadIO m => (a -> m a) -> IORef a -> m a
refreshed refresh ref = do
  old <- liftIO (readIORef ref)
  new <- refresh old
  liftIO (writeIORef ref new)
  pure new

banana ::
  SDL.Window -> Text -> FilePath -> Config ->
  Banana.AddHandler SDL.Event -> Banana.MomentIO ()
banana window text configPath initialConfig handler = do
  sdlE <- Banana.fromAddHandler handler
  let (press, scroll) = Banana.split $ Banana.filterJust $ sdlE <&> \e ->
        case e.eventPayload of
          SDL.KeyboardEvent ked
            | ked.keyboardEventKeyMotion == SDL.Pressed ->
              Just (Left ked.keyboardEventKeysym.keysymKeycode)
          SDL.MouseWheelEvent mwed -> Just $ Right
            (fromIntegral <$> mwed.mouseWheelEventPos)
          _ -> Nothing
      (exitKey, resize) = Banana.split $ Banana.filterJust $ press <&> \case
        SDL.KeycodeQ -> Just (Left ())
        SDL.KeycodeEquals -> Just (Right 1)
        SDL.KeycodeMinus -> Just (Right (-1))
        _ -> Nothing
      textLines = filter (not . Text.null . snd) $ zip [0..] (Text.lines text)
      scrollBounds = SDL.V2 (maximum (0 : map (Text.length . snd) textLines))
                            (length textLines)
  initialFontCache <- newFontCache initialConfig textLines
  fontCache <- liftIO $ newIORef initialFontCache
  position <- Banana.accumB (SDL.V2 0 0) $ scroll <&> updateSP scrollBounds
  config <- Banana.accumB initialConfig $ resize <&> \ds config -> config
    { fontSize = config.fontSize + ds }
  render <- Banana.changes $ renderAll fontCache <$> position <*> config
  Banana.reactimate' render
  onceExit <- Banana.once exitKey
  Banana.reactimate $ onceExit $> do
    lastFontCache <- readIORef fontCache
    freeCache lastFontCache
    _ <- Toml.encodeToFile Toml.genericCodec configPath lastFontCache.lastConfig
    exitSuccess
  where
    updateSP (SDL.V2 maxX maxY) (SDL.V2 dx dy) (SDL.V2 x y) = SDL.V2
      (clamp (0, maxX) (x + dx)) (clamp (0, maxY) (y - dy))
    renderAll fc (SDL.V2 colPos linePos) config = do
      windowSurface <- SDL.getWindowSurface window
      SDL.surfaceFillRect windowSurface Nothing (SDL.V4 0 0 0 255)
      fontCache <- refreshed (refreshCache config) fc
      lineSkip <- toEnum <$> TTF.lineSkip fontCache.lastFont
      colSkip <- case lookup
                        (fromIntegral linePos)
                        ((\(i,l,_) -> (i, l)) <$> fontCache.lastSurfaces) of
        Nothing -> pure 0
        Just line -> do
          let pos = fromIntegral colPos
              start = Text.take pos line
          (trueWidth, _) <- TTF.size fontCache.lastFont (Text.take pos line)
          Just (_, _, _, _, advance) <- TTF.glyphMetrics fontCache.lastFont 'o'
          pure $ trueWidth + advance * max 0 (pos - Text.length start)
      SDL.V2 _ windowHeight <- SDL.surfaceDimensions windowSurface
      for_ fontCache.lastSurfaces \(i, _, fontSurface) -> do
        let blitY = (i - toEnum linePos) * lineSkip
            blitPos = SDL.V2 (-toEnum colSkip) blitY
        if 0 <= blitY && blitY < windowHeight
        then SDL.surfaceBlit fontSurface Nothing windowSurface $
          Just (SDL.P blitPos)
        else pure Nothing
      SDL.updateWindowSurface window

main :: IO ()
main = Resource.runResourceT do
  _ <- TTF.initialize `Resource.allocate_` TTF.quit
  (_, window) <- SDL.createWindow "PrEd proof editor" SDL.defaultWindow
    { windowHighDPI = True
    , windowMode = SDL.Maximized
    , windowResizable = True
    } `Resource.allocate` SDL.destroyWindow
  liftIO do
    filePath <- Opt.execParser $ Opt.info
      (Opt.strArgument
        (Opt.metavar "FILE" <> Opt.help "File to edit" <> Opt.action "file")
          <**> Opt.helper)
      (Opt.fullDesc <> Opt.progDesc ("PrEd is a Proof Editor, "
        <> "an IDE specifically tailored for interactive proof assistants."))
    text <- Dir.doesFileExist filePath >>= \case
      True -> Text.readFile filePath
      False -> pure ""
    SDL.initializeAll
    configPath <- Dir.getXdgDirectory Dir.XdgConfig "predconfig.toml"
    config <- Dir.doesFileExist configPath >>= \case
      True -> Toml.decodeFile Toml.genericCodec configPath
      False -> do
        fc <- FC.initLoadConfigAndFonts
        pattern <- maybe (error "lol no Fira Code") pure $
          FC.fontMatch fc (FC.nameParse "Fira Code")
        fontPath <- maybe (error "lol no filepath") (pure . Text.pack) $
          FC.getValue "file" pattern
        pure MkConfig { fontSize = 36, .. }
    (addHandler, fire) <- Banana.newAddHandler
    network <- Banana.compile (banana window text configPath config addHandler)
    Banana.actuate network
    forever (SDL.waitEvent >>= fire)
