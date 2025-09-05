{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pred.UI (runUI) where

import Control.Exception qualified as E
import Control.Monad (forever, join)
import Control.Monad.Fix (mfix)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource qualified as Resource
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Ord (clamp)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Word (Word32, Word8)
import Graphics.Text.Font.Choose qualified as FC
import Reactive.Banana qualified as Banana
import Reactive.Banana.Frameworks qualified as Banana
import SDL qualified
import SDL.Raw qualified as Raw
import System.Directory qualified as Dir
import System.FilePath ((</>))
import qualified System.FilePath as FP
import System.Exit (exitSuccess)
import Toml qualified

import Pred.Prelude
import Pred.RichText qualified as Rich
import Pred.TTF qualified as TTF

-- | All thingies toml

-- | Keep TOML-friendly types; convert at runtime

data RGBA = RGBA { r :: Int, g :: Int, b :: Int, a :: Int }
  deriving (Generic, Eq, Show)

instance Toml.HasCodec RGBA where hasCodec key = Toml.table Toml.genericCodec key

data FontCfg = FontCfg { path :: Text, pointSize :: Int }
  deriving (Generic, Eq, Show)

instance Toml.HasCodec FontCfg where hasCodec key = Toml.table Toml.genericCodec key

data AppConfig = AppConfig
  { font      :: FontCfg
  , textColor :: RGBA
  , bgColor   :: RGBA
  } deriving (Generic, Eq, Show)

instance Toml.HasCodec AppConfig where hasCodec = const Toml.genericCodec

-- | Convert to runtime structures used by rendering

toSDL :: RGBA -> SDL.V4 Word8
toSDL RGBA{..} =
  let c x = fromIntegral (max 0 (min 255 x)) in SDL.V4 (c r) (c g) (c b) (c a)

toTTF :: FontCfg -> TTF.Font
toTTF FontCfg{..} = TTF.MkFont { path, pointSize = fromIntegral pointSize }

fromTTF :: TTF.Font -> FontCfg
fromTTF TTF.MkFont{..} = FontCfg { path, pointSize = fromIntegral pointSize }

-- | Where the cfg came from; we persist back to the same path

data CfgSource = FromRepo FilePath | FromXDG FilePath | Builtin

loadDefaultFont :: IO TTF.Font
loadDefaultFont = do
  fc <- FC.initLoadConfigAndFonts
  let prefs = ["Noto Sans Mono","Noto Sans","DejaVu Sans Mono","DejaVu Sans"]
  let pick []       = error "No suitable default font found via fontconfig (tried Noto/DejaVu)."
      pick (n:ns) = case FC.fontMatch fc (FC.nameParse n) >>= FC.getValue "file" of
        Nothing  -> pick ns
        Just pth -> pure (Text.pack pth)
  p <- pick prefs
  pure TTF.MkFont { pointSize = 36, path = p }

loadAppConfig :: IO (AppConfig, CfgSource)
loadAppConfig = do
  xdgDir   <- Dir.getXdgDirectory Dir.XdgConfig "pred"
  let xdgPath = xdgDir </> "predconfig.toml"
  xdgYes   <- Dir.doesFileExist xdgPath

  let decode p = Toml.decodeFile Toml.genericCodec p

  if xdgYes
    then (E.try (decode xdgPath) :: IO (Either E.SomeException AppConfig)) >>= \case
      Right cfg -> pure (cfg, FromXDG xdgPath)
      Left  _   -> tryRepoOrFallback
    else tryRepoOrFallback
  where
    tryRepoOrFallback = do
      repoExists <- Dir.doesFileExist "config.toml"
      if repoExists
        then (E.try (Toml.decodeFile Toml.genericCodec "config.toml") :: IO (Either E.SomeException AppConfig)) >>= \case
          Right cfg2 -> pure (cfg2, FromRepo "config.toml")
          Left  _    -> fallback
        else fallback
    fallback = do
      f <- loadDefaultFont
      pure ( AppConfig { font = fromTTF f
                       , textColor = RGBA 255 255 255 255
                       , bgColor   = RGBA   0   0   0 255 }
           , Builtin)
  

saveAppConfig :: CfgSource -> AppConfig -> IO ()
saveAppConfig src cfg = case src of
  FromRepo p -> do
    _ <- Toml.encodeToFile Toml.genericCodec p cfg
    pure ()
  FromXDG  p -> do
    let dir = FP.takeDirectory p
    Dir.createDirectoryIfMissing True dir
    _ <- Toml.encodeToFile Toml.genericCodec p cfg
    pure ()
  Builtin    -> pure ()

-- | Editor

data Mode = Normal | Edit deriving (Eq)

data Action = Move (Rich.VPC Int) | Enter Mode | ChangeFS Int | Exit

-- | Special editing keys handled in Edit mode

data Special = Backspace | Delete | Newline

data Editor = Editor { edLines :: [Text], edCursor :: SDL.Point Rich.VPC Int } deriving (Generic)

editorLines :: Editor -> [Text]
editorLines = edLines

editorCursor :: Editor -> SDL.Point Rich.VPC Int
editorCursor = edCursor

accumEditor
  :: Editor
  -> Banana.Behavior Mode
  -> Banana.Event Action
  -> Banana.Event Text
  -> Banana.Event SDL.Keycode
  -> Banana.MomentIO (Banana.Behavior Editor)
accumEditor initial modeB actions textInput press = do
  let moveE = Banana.filterJust $ actions <&> \case { Move dm -> Just dm; _ -> Nothing }
      inEditT :: Banana.Event Text -> Banana.Event Text
      inEditT eT = Banana.filterJust $ (\m x -> if m == Edit then Just x else Nothing)
                    <$> modeB Banana.<@> eT
      inEditS :: Banana.Event Special -> Banana.Event Special
      inEditS eS = Banana.filterJust $ (\m x -> if m == Edit then Just x else Nothing)
                    <$> modeB Banana.<@> eS
      specials = inEditS $ Banana.filterJust $ press <&> \case
        SDL.KeycodeBackspace -> Just Backspace
        SDL.KeycodeDelete    -> Just Delete
        SDL.KeycodeReturn    -> Just Newline
        _                    -> Nothing
      typed = inEditT textInput

      applyMove (Rich.VPC dc dl) ed =
        let SDL.P (Rich.VPC c l) = ed.edCursor
            lMax  = max 0 (length ed.edLines - 1)
            l'    = clamp (0, lMax) (l + dl)
            cMax  = Text.length (safeLine ed.edLines l')
            c'    = clamp (0, cMax) (c + dc)
        in ed { edCursor = SDL.P (Rich.VPC c' l') }

      insertChar ch ed | ch == '\n' = newline ed
      insertChar ch ed =
        let SDL.P (Rich.VPC c l) = ed.edCursor
            (pre, post) = Text.splitAt c (safeLine ed.edLines l)
            newLine = Text.snoc pre ch <> post
        in ed { edLines = setLine l newLine ed.edLines
              , edCursor = SDL.P (Rich.VPC (c + 1) l) }

      newline ed =
        let SDL.P (Rich.VPC c l) = ed.edCursor
            (pre, post) = Text.splitAt c (safeLine ed.edLines l)
            (before, after) = splitAt l ed.edLines
        in ed { edLines = before <> [pre, post] <> drop 1 after
              , edCursor = SDL.P (Rich.VPC 0 (l + 1)) }

      backspace ed =
        let SDL.P (Rich.VPC c l) = ed.edCursor in
        if c > 0 then
          let line = safeLine ed.edLines l
          in ed { edLines = setLine l (Text.take (c-1) line <> Text.drop c line) ed.edLines
                , edCursor = SDL.P (Rich.VPC (c-1) l) }
        else if l > 0 then
          let prev = safeLine ed.edLines (l-1)
              cur  = safeLine ed.edLines l
              (before, after) = splitAt (l-1) ed.edLines
          in ed { edLines = before <> [prev <> cur] <> drop 1 after
                , edCursor = SDL.P (Rich.VPC (Text.length prev) (l-1)) }
        else ed

      deleteForward ed =
        let SDL.P (Rich.VPC c l) = ed.edCursor
            line = safeLine ed.edLines l in
        if c < Text.length line then
          ed { edLines = setLine l (Text.take c line <> Text.drop (c+1) line) ed.edLines }
        else if l + 1 < length ed.edLines then
          let next = safeLine ed.edLines (l+1)
          in ed { edLines = setLine l (line <> next) (removeAt (l+1) ed.edLines) }
        else ed

      safeLine ls i | i < 0 || i >= length ls = ""
      safeLine ls i = ls !! i

      setLine i t ls = let (a,b) = splitAt i ls in case b of [] -> a<>[t]; (_:xs) -> a<>(t:xs)
      removeAt i ls  = take i ls <> drop (i+1) ls

      applyInsertText t ed = Text.foldl' (flip insertChar) ed t

      moveOps    = applyMove <$> moveE
      typeOps    = applyInsertText <$> typed
      specialOps = specials <&> \case { Backspace -> backspace; Delete -> deleteForward; Newline -> newline }
      allOps     = Banana.unions [moveOps, typeOps, specialOps]

  Banana.accumB initial allOps

-- | UI wiring -- will be a separate file in the future!

runUI :: FilePath -> IO ()
runUI filePath = Resource.runResourceT do
  SDL.initializeAll
  (_, window) <- SDL.createWindow "PrEd proof editor" SDL.defaultWindow
    { windowHighDPI = True
    , windowMode     = SDL.Maximized
    , windowResizable = True
    } `Resource.allocate` SDL.destroyWindow
  (_, fonts)  <- TTF.newFonts `Resource.allocate` TTF.closeFonts
  liftIO do
    (sdlHandler,   fireSDL)   <- Banana.newAddHandler
    (timerHandler, fireTimer) <- Banana.newAddHandler
    Banana.compile (banana filePath window fonts sdlHandler timerHandler) >>= Banana.actuate
    forever do
      SDL.waitEventTimeout 16 >>= maybe mempty fireSDL
      SDL.ticks >>= fireTimer

banana
  :: FilePath
  -> SDL.Window
  -> TTF.Fonts
  -> Banana.AddHandler SDL.Event
  -> Banana.AddHandler Word32
  -> Banana.MomentIO ()
banana filePath window fonts sdlHandler timerHandler = do
  text0 <- liftIO $ do
    exists <- Dir.doesFileExist filePath
    if exists then Text.readFile filePath else pure ""

  (initialCfg, cfgSrc) <- liftIO loadAppConfig
  liftIO $ do
    putStrLn $ case cfgSrc of
      FromXDG p -> "Config: XDG " <> p
      FromRepo p -> "Config: repo " <> p
      Builtin -> "Config: built-in defaults"
    putStrLn $ "Font: " <> Text.unpack (path (font initialCfg))
             <> ", size: " <> show (pointSize (font initialCfg))
  let initialFont      = toTTF initialCfg.font
      initialTextColor = toSDL initialCfg.textColor
      initialBGColor   = toSDL initialCfg.bgColor

  sdlE <- Banana.fromAddHandler sdlHandler
  time <- Banana.fromAddHandler timerHandler >>= Banana.stepper 0

  let (keyEv, scroll) = Banana.split $ Banana.filterJust $ sdlE <&> \e -> case SDL.eventPayload e of
        SDL.KeyboardEvent ked | SDL.keyboardEventKeyMotion ked == SDL.Pressed -> Just (Left ked)
        SDL.MouseWheelEvent mw                                               -> Just (Right (SDL.mouseWheelEventPos mw))
        _ -> Nothing

      unicodeInput :: Banana.Event Text
      unicodeInput = Banana.filterJust $ sdlE <&> \e -> case SDL.eventPayload e of
        SDL.TextInputEvent te -> Just (SDL.textInputEventText te)
        _                     -> Nothing

      clicks = Banana.filterJust $ sdlE <&> \e -> case SDL.eventPayload e of
        SDL.MouseButtonEvent mbed
          | SDL.mouseButtonEventMotion mbed == SDL.Pressed
          , SDL.mouseButtonEventWindow mbed == Just window
          -> Just (fromIntegral <$> SDL.mouseButtonEventPos mbed)
        _ -> Nothing

      keyActions ked =
        let kc   = SDL.keysymKeycode (SDL.keyboardEventKeysym ked)
            mods = SDL.keyboardEventKeysym ked
            ctrl = SDL.keyModifierLeftCtrl (SDL.keysymModifier mods)
                || SDL.keyModifierRightCtrl (SDL.keysymModifier mods)
        in case kc of
          SDL.KeycodeLeft   -> [Move (Rich.VPC (-1) 0)]
          SDL.KeycodeUp     -> [Move (Rich.VPC 0 (-1))]
          SDL.KeycodeDown   -> [Move (Rich.VPC 0 1)]
          SDL.KeycodeRight  -> [Move (Rich.VPC 1 0)]
          SDL.KeycodeEscape -> [Enter Normal]
          SDL.KeycodeReturn -> [Enter Edit]
          SDL.KeycodeMinus  -> [ChangeFS (-1) | ctrl]
          SDL.KeycodeEquals -> [ChangeFS 1     | ctrl]
          SDL.KeycodeQ      -> [Exit]
          _                 -> []

  let initialEditor = Editor { edLines = Text.lines text0, edCursor = SDL.P (Rich.VPC 0 0) }
  actions <- collect $ keyEv <&> keyActions

  let (exitKey, resize) = Banana.split $ Banana.filterJust $ actions <&> \case
        Exit        -> Just (Left ())
        ChangeFS ds -> Just (Right ds)
        _           -> Nothing

      (modes, moves) = Banana.split $ Banana.filterJust $ actions <&> \case
        Enter m -> Just (Left m)
        Move dm -> Just (Right dm)
        _       -> Nothing

  fontB <- Banana.accumB initialFont $ resize <&> \ds f -> f { pointSize = f.pointSize + ds }
  let textColorB = pure initialTextColor
      bgColorB   = pure initialBGColor

  modeB   <- Banana.stepper Normal modes
  editorB <- do
    let typedTextEv = unicodeInput
        keyCodeE    = keyEv <&> (SDL.keysymKeycode . SDL.keyboardEventKeysym)
    accumEditor initialEditor modeB actions typedTextEv keyCodeE

  let sourceB = Rich.sourceText . Text.unlines . editorLines <$> editorB

  let scrollDelta = scroll <&> (\(fmap fromEnum -> SDL.V2 dx dy) -> SDL.V2 dx dy)
      scrollUpdater = (\source (SDL.V2 dx dy) (SDL.P (Rich.VPC x y)) ->
                         Rich.clampToBox source $ SDL.P $ Rich.VPC (x + dx) (y - dy)) <$> sourceB
  scrollPos <- Banana.accumB (SDL.P $ Rich.VPC 0 0) $ scrollUpdater Banana.<@> scrollDelta

  viewPort <- mfix $ \vport -> do
    clickPos <- Banana.mapEventIO (\(vp,pos) -> Rich.pxToViewPort vp fonts pos) ((,) <$> vport Banana.<@> clicks)
    let cursorActions = Banana.unions
          [ (\pos _ -> pos) <$> clickPos
          , (\source dm -> Rich.moveViewPort source dm) <$> sourceB Banana.<@> moves
          ]
    latB <- Banana.stepper 0 (cursorActions Banana.@> time)
    let drawCursorB = (\t lat -> (t - lat) `mod` 1000 < 500) <$> time <*> latB
    let textMarkers = do
          mode <- modeB
          cur  <- editorCursor <$> editorB
          blink <- drawCursorB
          pure $ case mode of
            Edit   -> ([], [cur | blink])
            Normal -> ([cur], [])
    pure $ do
      font      <- fontB
      position  <- scrollPos
      source    <- sourceB
      textColor <- textColorB
      bgColor   <- bgColorB
      (selection, cursors) <- textMarkers
      pure Rich.TextViewPort { source, font, textColor, bgColor, position, selection, cursors }

  let renderer = viewPort <&> \tvp -> do
        windowSurface <- SDL.getWindowSurface window
        Rich.blitTextViewPort windowSurface fonts tvp
        SDL.updateWindowSurface window

  -- | Toggle SDL text input when entering/leaving edit mode.
  Banana.reactimate $ modes <&> \m -> case m of { Edit -> Raw.startTextInput; Normal -> Raw.stopTextInput }
  Banana.changes renderer >>= Banana.reactimate'
  Banana.valueB  renderer >>= liftIO

  onceExit <- Banana.once exitKey
  Banana.reactimate $ onceExit Banana.@> fontB <&> \f -> do
    let outCfg = initialCfg { font = fromTTF f }
    saveAppConfig cfgSrc outCfg
    exitSuccess

collect :: Foldable f => Banana.Event (f a) -> Banana.MomentIO (Banana.Event a)
collect events = do
  (h, fire) <- liftIO Banana.newAddHandler
  Banana.reactimate $ events <&> traverse_ fire
  Banana.fromAddHandler h
