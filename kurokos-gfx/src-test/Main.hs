{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Control.Exception             as E
import           Control.Monad                 (unless)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Managed         (managed, runManaged)
import           Linear.V2

import qualified SDL
import           SDL.Event

import qualified Graphics.GLUtil               as GLU
import           Graphics.Rendering.OpenGL     (get, ($=))
import qualified Graphics.Rendering.OpenGL     as GL

import qualified Kurokos.Graphics.Text         as GText
import qualified Kurokos.Graphics.Shader       as G
import qualified Kurokos.Graphics.Render       as G
import qualified Kurokos.Graphics.Shader.Basic as SB
import qualified Kurokos.Graphics.Shader.Text  as ST
import qualified Kurokos.Graphics.Texture      as G

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Test kurokos-gfx" winConf
  withGL window $ \_glContext -> do
    -- GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    SDL.swapInterval $= SDL.SynchronizedUpdates
    GL.clearColor $= GL.Color4 0 1 0 1
    --
    runManaged $ do
      ft <- managed GText.withFreeType
      face <- managed $ E.bracket (GText.newFace ft "_test/mplus-1p-medium.ttf") GText.doneFace
      liftIO $ GText.setPixelSize face 32
    --
      texttex <- managed $
                  E.bracket (GText.createTextTexture face "Hello, World!")
                            GText.deleteTextTexture
      liftIO $ do
        winSize <- get $ SDL.windowSize window
        br <- SB.newBasicRenderer
        G.updateProjection G.Ortho winSize br
        st <- ST.newTextShader
        G.updateProjection G.Ortho winSize st
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
          G.setTexture br $ if i `mod` 60 < 30 then tex1 else tex2
          G.renderByShader_ br ctx
          --
          G.renderText (V2 100 240) st texttex
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
