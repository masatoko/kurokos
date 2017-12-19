{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Monad (unless)
import qualified SDL
import           SDL.Event

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Test kurokos-gfx" winConf
  loop window
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
          --
          unless (any shouldExit events) go

    shouldExit e =
      case SDL.eventPayload e of
        KeyboardEvent KeyboardEventData{..} ->
          keyboardEventKeyMotion == Pressed &&
            SDL.keysymKeycode keyboardEventKeysym == SDL.KeycodeQ
        _ -> False
