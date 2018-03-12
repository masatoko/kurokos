{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Exception         (bracket)
import           Control.Monad             (unless)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Managed     (managed, runManaged)

import qualified SDL
import           SDL.Event

import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (get, ($=))
import qualified Graphics.Rendering.OpenGL as GL

import qualified Kurokos.Graphics          as G
import qualified Kurokos.Graphics.Font     as Font
import           Kurokos.Graphics.Vect

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Test kurokos-gfx" winConf
  withGL window $ \_glContext -> do
    winSize@(V2 winW winH) <- get $ SDL.windowSize window
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral winW) (fromIntegral winH))
    SDL.swapInterval $= SDL.SynchronizedUpdates
    --
    runManaged $ do
      rndr <- managed $ bracket (G.newRenderer winSize) G.freeRenderer

      ft <- managed Font.withFreeType
      face <- managed $ bracket (Font.newFace ft "_test/mplus-1p-medium.ttf") Font.doneFace
      --
      text1 <- managed $
                bracket (G.createTextTexture face 32 (V4 255 0 0 255) "Hello, ") G.deleteTextTexture
      text2 <- managed $
                bracket (G.createTextTexture face 32 (V4 0 0 255 255) "World!") G.deleteTextTexture
      let texttex = text1 ++ text2
      helloTex <- managed $
                    bracket (G.genTextImage rndr texttex) G.deleteTexture

      tile <- managed $ bracket (G.readTexture "_data/tile.png") G.deleteTexture
      tex1 <- managed $ bracket (G.readTexture "_data/in_transit.png") G.deleteTexture
      tex2 <- managed $ bracket (G.readTexture "_data/panorama.png") G.deleteTexture
      let ps = map (P . uncurry V2) [(0,0), (100,0), (50,100)]
      poly <- managed $ bracket (G.newPrim rndr GL.LineLoop ps) G.freePrim
      rect <- managed $ bracket (G.newRectangle rndr (V2 40 20)) G.freePrim
      fillRect <- managed $ bracket (G.newFillRectangle rndr (V2 40 20)) G.freePrim
      --
      liftIO $ loop window rndr tile tex1 tex2 texttex helloTex poly rect fillRect
  where
    winConf =
      SDL.defaultWindow
        { SDL.windowOpenGL = Just glConf
        , SDL.windowInitialSize = V2 640 480}

    withGL win =
      bracket (SDL.glCreateContext win)
              SDL.glDeleteContext

    glConf =
      SDL.defaultOpenGL
        { SDL.glProfile = SDL.Core SDL.Debug 4 0
        }

    loop win rndr tile tex1 tex2 texttex helloTex poly rect fillRect = go (0 :: Integer)
      where
        go i = do
          let i' = fromIntegral i
          GLU.printError
          events <- SDL.pollEvent
          GL.clearColor $= GL.Color4 0.2 0.2 0.2 1
          GL.clear [GL.ColorBuffer]
          --
          let ctxTile = G.RContext (pure 10) (pure 128) Nothing Nothing
          G.renderTexture rndr tile (Just (P (V2 16 0), pure 16)) ctxTile
          --
          let ctx = G.RContext (pure 0) (pure i') Nothing Nothing
              tex = if i `mod` 60 < 30 then tex1 else tex2
          G.renderTexture rndr tex Nothing ctx
          --
          G.renderText rndr (V2 100 0) texttex
          G.renderText rndr (V2 100 480) texttex
          G.renderTexture rndr helloTex Nothing (G.RContext (V2 10 100) (V2 (G.texWidth helloTex) (G.texHeight helloTex)) Nothing Nothing)
          --
          G.drawPrim rndr (V2 400 200) poly
          --
          G.setPrimColor rndr $ V4 255 255 255 255
          G.drawPrim rndr (V2 400 300) fillRect
          G.setPrimColor rndr $ V4 255 0 0 255
          G.drawPrim rndr (V2 400 300) rect
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
