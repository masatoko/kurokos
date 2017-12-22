{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Control.Exception             as E
import           Control.Monad                 (unless)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Managed         (managed, runManaged)
import           Data.Either.Extra             (fromRight)
import qualified Data.Vector                   as V
import           Foreign.Storable              (sizeOf)
import           Linear.V2
import           Linear.V3
import           System.FilePath.Posix

import qualified SDL
import           SDL.Event

import qualified Graphics.GL                   as GLRaw
import qualified Graphics.GLUtil               as GLU
import           Graphics.Rendering.OpenGL     (get, ($=))
import qualified Graphics.Rendering.OpenGL     as GL

import qualified Kurokos.Graphics.Font         as Font
import qualified Kurokos.Graphics.Shader       as KG
import qualified Kurokos.Graphics.Shader.Basic as SB
import qualified Kurokos.Graphics.Shader.Text  as ST
import qualified Kurokos.Graphics.Texture      as KG
import qualified Kurokos.Graphics.Texture
import qualified Kurokos.Graphics.Types        as KG

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Test kurokos-gfx" winConf
  withGL window $ \glContext -> do
    -- GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    SDL.swapInterval $= SDL.SynchronizedUpdates
    GL.clearColor $= GL.Color4 0 1 0 1
    --
    runManaged $ do
      ft <- managed Font.withFreeType
      face <- managed $ E.bracket (Font.newFace ft "_test/mplus-1p-medium.ttf") Font.doneFace
      liftIO $ Font.setPixelSize face 32
    --
      texttex <- managed $
                  E.bracket (Font.createTextTexture face "Hello, World!")
                            Font.deleteTextTexture
      liftIO $ do
        winSize <- get $ SDL.windowSize window
        br <- SB.newBasicRenderer
        SB.updateBasicRenderer KG.Ortho winSize br
        st <- ST.newTextShader
        ST.updateTextShader KG.Ortho winSize st
        --
        tex1 <- KG.readTexture "_data/in_transit.png"
        tex2 <- KG.readTexture "_data/panorama.png"
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

    loop win br st tex1 tex2 texttex = go 0
      where
        go i = do
          let i' = fromIntegral i
          GLU.printError
          events <- SDL.pollEvent
          GL.clear [GL.ColorBuffer]
          --
          let ctx = KG.RContext (V2 320 240) (Just (pure i')) (Just $ i' / 10) Nothing
          SB.renderTexByBasicRenderer_ br ctx $ if i `mod` 60 < 30 then tex1 else tex2
          let x0 = 100
              y = 240
              work x (KG.CharTexture tex left top dx _ offY) = do
                let x' = x + fromIntegral left
                    y' = y + fromIntegral offY
                let ctx = KG.RContext (V2 x' y') Nothing Nothing Nothing
                -- SB.renderTexByBasicRenderer_ br ctx tex
                ST.renderTexByBasicShader_ st ctx tex
                return $ x + dx
          ST.setColor st $ V3 (i `mod` 255) 0 (255 - i `mod` 255)
          V.foldM_ work x0 texttex
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
