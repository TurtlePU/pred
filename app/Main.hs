{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Control.Applicative ((<**>))
import Control.Monad (forever)
import Control.Monad.Fix (mfix)
import Data.Functor ((<&>))
import Data.Ord (clamp)
import Data.Word (Word32)
import System.Exit (exitSuccess)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource qualified as Resource
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Graphics.Text.Font.Choose qualified as FC
import Options.Applicative qualified as Opt
import Reactive.Banana qualified as Banana
import Reactive.Banana.Frameworks qualified as Banana
import SDL qualified
import System.Directory qualified as Dir
import Toml qualified

import Pred.Prelude
import Pred.RichText qualified as Rich
import Pred.TTF qualified as TTF

data Mode = Normal | Edit

main :: IO ()
main = Resource.runResourceT do
  SDL.initializeAll
  (_, window) <- SDL.createWindow "PrEd proof editor" SDL.defaultWindow
    { windowHighDPI = True
    , windowMode = SDL.Maximized
    , windowResizable = True
    } `Resource.allocate` SDL.destroyWindow
  (_, fonts) <- TTF.newFonts `Resource.allocate` TTF.closeFonts
  liftIO do
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
    False -> pure ""
  configPath <- liftIO $ Dir.getXdgDirectory Dir.XdgConfig "predconfig.toml"
  initialFont <- liftIO $ Dir.doesFileExist configPath >>= \case
    True -> Toml.decodeFile Toml.genericCodec configPath
    False -> do
      fc <- FC.initLoadConfigAndFonts
      pattern <- maybe (error "lol no Fira Code") pure $
        FC.fontMatch fc (FC.nameParse "Fira Code")
      path <- maybe (error "lol no filepath") (pure . Text.pack) $
        FC.getValue "file" pattern
      pure TTF.MkFont { pointSize = 36, .. }
  sdlE <- Banana.fromAddHandler sdlHandler
  time <- Banana.fromAddHandler timerHandler >>= Banana.stepper 0
  let (press, scroll) = Banana.split $ Banana.filterJust $ sdlE <&> \e ->
        case e.eventPayload of
          SDL.KeyboardEvent ked
            | ked.keyboardEventKeyMotion == SDL.Pressed ->
              Just (Left ked.keyboardEventKeysym.keysymKeycode)
          SDL.MouseWheelEvent mwed -> Just $ Right
            (fromIntegral <$> mwed.mouseWheelEventPos)
          _ -> Nothing
      clicks = Banana.filterJust $ sdlE <&> \e ->
        case e.eventPayload of
          SDL.MouseButtonEvent mbed
            | mbed.mouseButtonEventMotion == SDL.Pressed
              && mbed.mouseButtonEventWindow == Just window ->
                Just (fromIntegral <$> mbed.mouseButtonEventPos)
          _ -> Nothing
      modeE = Banana.filterJust $ press <&> \case
        SDL.KeycodeEscape -> Just Normal
        SDL.KeycodeReturn -> Just Edit
        _ -> Nothing
      (exitKey, resize) = Banana.split $ Banana.filterJust $ press <&> \case
        SDL.KeycodeQ -> Just (Left ())
        SDL.KeycodeEquals -> Just (Right 1)
        SDL.KeycodeMinus -> Just (Right (-1))
        _ -> Nothing
      source = Rich.sourceText text
      makeViewPort font position drawCursor mode cursorPos =
        let (selection, cursors) = case mode of
              Edit -> if drawCursor then ([], [cursorPos]) else mempty
              Normal -> ([cursorPos], [])
         in Rich.TextViewPort
            { bgColor = SDL.V4 0 0 0 255
            , textColor = SDL.V4 255 255 255 255
            , .. }
  font <- Banana.accumB initialFont $ resize <&>
    \ds font -> font { pointSize = font.pointSize + ds }
  scrollPos <- Banana.accumB (SDL.P $ Rich.VPC 0 0) $
    scroll <&> \(SDL.V2 dx dy) (SDL.P (Rich.VPC x y)) ->
      let Rich.VPC maxX maxY = Rich.boundingBox source
       in SDL.P $ clamp (0, maxX) (x + dx) `Rich.VPC` clamp (0, maxY) (y - dy)
  drawCursor <- Banana.stepper 0 (clicks Banana.@> time)
    <&> liftA2 (\t lct -> (t - lct) `mod` 1000 < 500) time
  mode <- Banana.stepper Normal modeE
  let viewPort0 = makeViewPort <$> font <*> scrollPos <*> drawCursor <*> mode
  viewPort <- mdo
    cursorPos <- Banana.mapEventIO
      (\(vp, pos) -> Rich.pxToViewPort vp fonts pos)
      ((,) <$> vport Banana.<@> clicks)
      >>= Banana.stepper (SDL.P $ Rich.VPC 0 0)
    let vport = viewPort0 <*> cursorPos
    pure vport
  let renderer = viewPort <&> \tvp -> do
        windowSurface <- SDL.getWindowSurface window
        Rich.blitTextViewPort windowSurface fonts tvp
        SDL.updateWindowSurface window
  Banana.changes renderer >>= Banana.reactimate'
  Banana.valueB renderer >>= liftIO
  onceExit <- Banana.once exitKey
  Banana.reactimate $ onceExit Banana.@> font <&> \f -> do
    _ <- Toml.encodeToFile Toml.genericCodec configPath f
    exitSuccess
