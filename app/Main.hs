{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Control.Applicative ((<**>))
import Control.Exception (bracket)
import Control.Monad (forever)
import Control.Monad.Fix (mfix)
import Data.Foldable (traverse_)
import Data.String qualified as String
import Data.Functor ((<&>))
import Data.Word (Word32)
import System.Exit (exitSuccess)

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Graphics.Text.Font.Choose qualified as FC
import Options.Applicative qualified as Opt
import Reactive.Banana qualified as Banana
import Reactive.Banana.Frameworks qualified as Banana
import SDL qualified
import System.Directory qualified as Dir
import Toml qualified

import Pred.BoundingBox qualified as BB
import Pred.RichText qualified as Rich
import Pred.SourceText qualified as Source
import Pred.TTF qualified as TTF

data Mode = Normal | Edit deriving (Bounded, Enum, Eq)

data Action = Move (Source.VPC Int) | Enter Mode | ChangeFS Int | Exit

main :: IO ()
main = do
  SDL.initializeAll
  let newWindow = SDL.createWindow (String.fromString "PrEd proof editor")
        SDL.defaultWindow
          { SDL.windowHighDPI = True
          , SDL.windowMode = SDL.Maximized
          , SDL.windowResizable = True
          }
      withWindow = newWindow `bracket` SDL.destroyWindow
      withFonts = TTF.newFonts `bracket` TTF.closeFonts
  withWindow \window -> withFonts \fonts -> do
    (sdlHandler, fireSDL) <- Banana.newAddHandler
    (timerHandler, fireTimer) <- Banana.newAddHandler
    Banana.compile (banana window fonts sdlHandler timerHandler)
      >>= Banana.actuate
    forever do
      SDL.waitEventTimeout 16 >>= maybe mempty fireSDL
      SDL.ticks >>= fireTimer

banana ::
  SDL.Window -> TTF.Fonts ->
  Banana.AddHandler SDL.Event -> Banana.AddHandler Word32 -> Banana.MomentIO ()
banana window fonts sdlHandler timerHandler = do
  filePath <- liftIO $ Opt.execParser $ Opt.info
    (Opt.strArgument
      (Opt.metavar "FILE" <> Opt.help "File to edit" <> Opt.action "file")
        <**> Opt.helper)
    (Opt.fullDesc <> Opt.progDesc ("PrEd is a Proof Editor, "
      <> "an IDE specifically tailored for interactive proof assistants."))
  text <- liftIO $ Dir.doesFileExist filePath >>= \case
    True -> Text.readFile filePath
    False -> pure Text.empty
  configPath <- liftIO $ Dir.getXdgDirectory Dir.XdgConfig "predconfig.toml"
  initialFont <- liftIO $ Dir.doesFileExist configPath >>= \case
    True -> Toml.decodeFile Toml.genericCodec configPath
    False -> do
      fc <- FC.initLoadConfigAndFonts
      pattern <- maybe (error "lol no Fira Code") pure $
        FC.fontMatch fc (FC.nameParse "Fira Code")
      path <- maybe (error "lol no filepath") (pure . Text.pack) $
        FC.getValue (String.fromString "file") pattern
      pure TTF.MkFont { pointSize = 36, path = path }
  sdlE <- Banana.fromAddHandler sdlHandler
  time <- Banana.fromAddHandler timerHandler >>= Banana.stepper 0
  let (press, scroll) = Banana.split $ Banana.filterJust $ sdlE <&> \e ->
        case e.eventPayload of
          SDL.KeyboardEvent ked
            | ked.keyboardEventKeyMotion == SDL.Pressed ->
              Just (Left ked.keyboardEventKeysym.keysymKeycode)
          SDL.MouseWheelEvent mwed ->
              Just (Right mwed.mouseWheelEventPos)
          _ -> Nothing
      clicks = Banana.filterJust $ sdlE <&> \e ->
        case e.eventPayload of
          SDL.MouseButtonEvent mbed
            | mbed.mouseButtonEventMotion == SDL.Pressed
              && mbed.mouseButtonEventWindow == Just window ->
                Just (fromIntegral <$> mbed.mouseButtonEventPos)
          _ -> Nothing
      actionMap =
        [ (Move (Source.VPC (-1) 0), [minBound..maxBound], SDL.KeycodeLeft)
        , (Move (Source.VPC 0 (-1)), [minBound..maxBound], SDL.KeycodeUp)
        , (Move (Source.VPC 0 1), [minBound..maxBound], SDL.KeycodeDown)
        , (Move (Source.VPC 1 0), [minBound..maxBound], SDL.KeycodeRight)
        , (Enter Normal, [Edit], SDL.KeycodeEscape)
        , (Enter Edit, [Normal], SDL.KeycodeReturn)
        , (ChangeFS (-1), [Normal], SDL.KeycodeMinus)
        , (ChangeFS 1, [Normal], SDL.KeycodeEquals)
        , (Exit, [Normal], SDL.KeycodeQ)
        ]
      source = Source.sourceText text
  (actions, modes) <- mfix \ ~(_, modes) -> do
    actions' <- collect $
        (\md kc -> [ ac | (ac, ms, k) <- actionMap, k == kc, md `elem` ms ])
        <$> modes Banana.<@> press
    modes' <- Banana.stepper Normal $ Banana.filterJust $ actions' <&> \case
      Enter mode -> Just mode; _ -> Nothing
    pure (actions', modes')
  let (exitKey, resize) = Banana.split $ Banana.filterJust $ actions <&> \case
        Exit -> Just (Left ())
        ChangeFS ds -> Just (Right ds)
        _ -> Nothing
      moves = Banana.filterJust $ actions <&> \case
        Move dm -> Just dm; _ -> Nothing
  fontB <- Banana.accumB initialFont $ resize <&>
    \ds font -> font { TTF.pointSize = font.pointSize + ds }
  scrollPos <- Banana.accumB (SDL.P $ Source.VPC 0 0) $
    scroll <&> \(fmap fromEnum -> SDL.V2 dx dy) (SDL.P (Source.VPC x y)) ->
      BB.clampToBox (Source.boundingBox source) $
        SDL.P $ Source.VPC (x + dx) (y - dy)
  viewPort <- mfix \vport -> do
    clickPos <- Banana.mapEventIO
      (\(vp, pos) -> Rich.pxToViewPort vp fonts pos)
      ((,) <$> vport Banana.<@> clicks)
    let cursorActions = Banana.unions
          [ const <$> clickPos
          , Source.moveViewPort source <$> moves
          ]
    cursorPosB <- Banana.accumB (SDL.P $ Source.VPC 0 0) cursorActions
    drawCursorB <- liftA2 (\t lat -> (t - lat) `mod` 1000 < 500) time
      <$> Banana.stepper 0 (cursorActions Banana.@> time)
    let textManipulators = do
          mode <- modes
          cursorPos <- cursorPosB
          drawCursor <- drawCursorB
          pure case mode of
            Edit -> ([], [cursorPos | drawCursor])
            Normal -> ([cursorPos], [])
    pure do
      font <- fontB
      position <- scrollPos
      (selection, cursors) <- textManipulators
      pure Rich.TextViewPort
        { source = source
        , font = font
        , bgColor = SDL.V4 0 0 0 255
        , textColor = SDL.V4 255 255 255 255
        , position = position
        , selection = selection
        , cursors = cursors
        }
  let renderer = viewPort <&> \tvp -> do
        windowSurface <- SDL.getWindowSurface window
        Rich.blitTextViewPort windowSurface fonts tvp
        SDL.updateWindowSurface window
  Banana.changes renderer >>= Banana.reactimate'
  Banana.valueB renderer >>= liftIO
  onceExit <- Banana.once exitKey
  Banana.reactimate $ onceExit Banana.@> fontB <&> \f -> do
    _ <- Toml.encodeToFile Toml.genericCodec configPath f
    exitSuccess

collect :: Foldable f => Banana.Event (f a) -> Banana.MomentIO (Banana.Event a)
collect events = do
  (handler, fire) <- liftIO Banana.newAddHandler
  Banana.reactimate $ events <&> traverse_ fire
  Banana.fromAddHandler handler
