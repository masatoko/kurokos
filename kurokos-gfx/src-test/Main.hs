{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Control.Exception             as E
import           Control.Monad                 (unless)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Managed         (managed, runManaged)
import           Linear.V2
import           Linear.V3

import qualified SDL
import           SDL.Event

import qualified Graphics.GLUtil               as GLU
import           Graphics.Rendering.OpenGL     (get, ($=))
import qualified Graphics.Rendering.OpenGL     as GL

import qualified Kurokos.Graphics              as G
import qualified Kurokos.Graphics.Camera       as Cam
import qualified Kurokos.Graphics.Font         as Font

-- TODO: Remove these shader modules
import qualified Kurokos.Graphics.Shader       as G
import qualified Kurokos.Graphics.Shader.Basic as SB
import qualified Kurokos.Graphics.Shader.Text  as ST

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Test kurokos-gfx" winConf
  withGL window $ \_glContext -> do
    winSize@(V2 winW winH) <- get $ SDL.windowSize window
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral winW) (fromIntegral winH))
    SDL.swapInterval $= SDL.SynchronizedUpdates
    GL.clearColor $= GL.Color4 0 1 0 1
    --
    runManaged $ do
      ft <- managed Font.withFreeType
      face <- managed $ E.bracket (Font.newFace ft "_test/mplus-1p-medium.ttf") Font.doneFace
      liftIO $ Font.setPixelSize face 32
    --
      text1 <- managed $
                E.bracket (G.createTextTexture face (V3 255 0 0) "Hello, ") G.deleteTextTexture
      text2 <- managed $
                E.bracket (G.createTextTexture face (V3 0 0 255) "World!") G.deleteTextTexture
      let texttex = text1 ++ text2

      liftIO $ do
        br <- SB.newBasicShader
        G.setProjection G.Ortho winSize br
        st <- ST.newTextShader
        G.setProjection G.Ortho winSize st
        --
        tex1 <- G.readTexture "_data/in_transit.png"
        tex2 <- G.readTexture "_data/panorama.png"
        loop window br st tex1 tex2 texttex
  where
    winConf =
      SDL.defaultWindow
        { SDL.windowOpenGL = Just glConf
        , SDL.windowInitialSize = V2 640 480}

    withGL win =
      E.bracket (SDL.glCreateContext win)
                SDL.glDeleteContext

    glConf =
      SDL.defaultOpenGL
        { SDL.glProfile = SDL.Core SDL.Debug 4 0
        }

    loop win br st tex1 tex2 texttex = go (0 :: Integer)
      where
        go i = do
          let i' = fromIntegral i
          GLU.printError
          events <- SDL.pollEvent
          GL.clear [GL.ColorBuffer]
          --
          let ctx = G.RContext (V2 320 240) (pure i') (Just $ i' / 10) Nothing
          G.setTexture br $ G.texObject $ if i `mod` 60 < 30 then tex1 else tex2
          G.renderByShader br Cam.mkCamera ctx
          --
          G.renderTextTexture st (V2 100 240) texttex
          --
          SDL.glSwapWindow win
          unless (any shouldExit events) $ go (i + 1)

    shouldExit e =
      case SDL.eventPayload e of
        KeyboardEvent KeyboardEventData{..} ->
          keyboardEventKeyMotion == Pressed
            && SDL.keysymKeycode keyboardEventKeysym `elem` [SDL.KeycodeQ, SDL.KeycodeEscape]
        WindowClosedEvent WindowClosedEventData{} -> True
        _ -> False
