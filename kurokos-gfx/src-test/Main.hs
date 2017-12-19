{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Monad             (unless)
import qualified SDL
import           SDL.Event

import qualified Graphics.GL               as GLRaw
import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (get, ($=))
import qualified Graphics.Rendering.OpenGL as GL

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Test kurokos-gfx" winConf
  glContext <- SDL.glCreateContext window
  SDL.swapInterval $= SDL.SynchronizedUpdates
  GL.clearColor $= GL.Color4 0 1 0 1
  loop window
  SDL.glDeleteContext glContext
  where
    winConf = SDL.defaultWindow {SDL.windowOpenGL = Just glConf}

    glConf =
      SDL.defaultOpenGL
        { SDL.glProfile = SDL.Core SDL.Debug 4 0
        }

    loop w = go
      where
        go = do
          events <- SDL.pollEvent
          --
          GL.clear [GL.ColorBuffer]
          SDL.glSwapWindow w
          --
          unless (any shouldExit events) go

    shouldExit e =
      case SDL.eventPayload e of
        KeyboardEvent KeyboardEventData{..} ->
          keyboardEventKeyMotion == Pressed &&
            SDL.keysymKeycode keyboardEventKeysym == SDL.KeycodeQ
        WindowClosedEvent WindowClosedEventData{} -> True
        _ -> False
