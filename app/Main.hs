module Main (main) where

import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL

import Graphics.GL
import SDL

import Control.Monad.Managed
import Control.Monad.IO.Class ()
import Control.Monad (when)
import Control.Exception (bracket, bracket_)
import Data.Text (Text)

main :: IO ()
main = ("PrEd proof editor" `renderedWhile` notQuitting) do
  withWindowOpen "Hello, ImGui!" do
    text "Hello, ImGui!"
    button "Clickety Click" >>= \clicked ->
      when clicked $ putStrLn "Ow!"
  showDemoWindow
 where
  notQuitting = all ((/= QuitEvent) . eventPayload) <$> pollEventWithImGui

renderedWhile :: Text -> IO Bool -> IO () -> IO ()
renderedWhile windowName cond body = initializeAll >> runManaged do
  window <- managed $ flip bracket destroyWindow $
    createWindow windowName defaultWindow
      { windowGraphicsContext = OpenGLContext defaultOpenGL }
  glContext <- managed $ bracket (glCreateContext window) glDeleteContext
  _ <- managed (bracket createContext destroyContext)
  managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
  managed_ (bracket_ openGL3Init openGL3Shutdown)
  liftIO $ while cond do
    openGL3NewFrame >> sdl2NewFrame >> newFrame
    body
    glClear GL_COLOR_BUFFER_BIT >> render
    getDrawData >>= openGL3RenderDrawData
    glSwapWindow window

while :: Monad m => m Bool -> m a -> m ()
while cond body = go
 where
  go = cond >>= \case
    True -> body >> go
    False -> pure ()
