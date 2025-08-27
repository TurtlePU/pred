module Main (main) where

import Control.Applicative ((<**>))
import Control.Monad (forever, when)
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

import Pred.DisplayedText qualified as Displayed
import Pred.Prelude
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
      textLines = Displayed.displayedText text
      scrollBounds = Displayed.boundingBox textLines
  time <- Banana.fromAddHandler timerHandler >>= Banana.stepper 0
  lastClickTime <- Banana.stepper 0 $ clicks Banana.@> time
  let drawCursor = (\t lct -> (t - lct) `mod` 1000 < 500) <$> time <*> lastClickTime
  position <- Banana.accumB (SDL.P $ Displayed.VPC 0 0) $ scroll <&> updateSP scrollBounds
  font <- Banana.accumB initialFont $ resize <&>
    \ds font -> font { pointSize = font.pointSize + ds }
  clickPos <- Banana.mapEventIO id $
    findClickPos textLines <$> position <*> font Banana.<@> clicks
  click <- Banana.stepper (SDL.P $ Displayed.VPC 0 0) clickPos
  mode <- Banana.stepper Normal modeE
  let renderer =
        renderAll textLines <$> position
                            <*> font
                            <*> drawCursor
                            <*> click
                            <*> mode
  Banana.changes renderer >>= Banana.reactimate'
  Banana.valueB renderer >>= liftIO
  onceExit <- Banana.once exitKey
  Banana.reactimate $ onceExit Banana.@> font <&> \f -> do
    _ <- Toml.encodeToFile Toml.genericCodec configPath f
    exitSuccess
  where
    updateSP (Displayed.VPC maxX maxY) (SDL.V2 dx dy)
             (SDL.P (Displayed.VPC x y)) = SDL.P $
      clamp (0, maxX) (x + dx) `Displayed.VPC` clamp (0, maxY) (y - dy)

    findClickPos sourceText position font clickPx = do
      fc <- TTF.load fonts font
      Displayed.pxToViewPort Displayed.TextViewPort {..} fc clickPx

    renderAll sourceText position font drawCursor cursorPos mode = do
      windowSurface <- SDL.getWindowSurface window
      SDL.surfaceFillRect windowSurface Nothing (SDL.V4 0 0 0 255)
      fontCache <- TTF.load fonts font
      Displayed.blitVisibleText windowSurface fontCache (SDL.V4 255 255 255 255)
        Displayed.TextViewPort {..}
      case mode of
        Normal -> Displayed.blitSelection windowSurface fontCache
                    (SDL.V4 255 255 255 255) Displayed.TextViewPort {..}
                    cursorPos (SDL.V4 0 0 0 255)
        Edit -> when drawCursor $
          Displayed.blitCursor windowSurface fontCache (SDL.V4 255 255 255 255)
            Displayed.TextViewPort {..} cursorPos
      SDL.updateWindowSurface window
