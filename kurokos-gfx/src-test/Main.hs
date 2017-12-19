{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Control.Exception         as E
import           Control.Monad             (unless)
import           Data.Either.Extra         (fromRight)
import           Foreign.Storable          (sizeOf)
import           System.FilePath.Posix

import qualified SDL
import           SDL.Event

import qualified Graphics.GL               as GLRaw
import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (get, ($=))
import qualified Graphics.Rendering.OpenGL as GL

import qualified Kurokos.Graphics.Texture  as KG

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Test kurokos-gfx" winConf
  withGL window $ \glContext -> do
    SDL.swapInterval $= SDL.SynchronizedUpdates
    -- GL.doubleBuffer
    GL.clearColor $= GL.Color4 0 1 0 1
    loop window
  where
    winConf = SDL.defaultWindow {SDL.windowOpenGL = Just glConf}

    withGL win =
      E.bracket (SDL.glCreateContext win)
                SDL.glDeleteContext

    glConf =
      SDL.defaultOpenGL
        { SDL.glProfile = SDL.Core SDL.Debug 3 0
        }

    loop w = do
      --
      r <- makeMyResource
      s <- makeMyShader
      ready s r
      --
      GLU.printError
      --
      go
      where
        go = do
          GLU.printError
          events <- SDL.pollEvent
          GL.clear [GL.ColorBuffer]
          --
          GL.drawElements GL.TriangleStrip 4 GL.UnsignedInt GLU.offset0
          --
          SDL.glSwapWindow w
          unless (any shouldExit events) go

    shouldExit e =
      case SDL.eventPayload e of
        KeyboardEvent KeyboardEventData{..} ->
          if keyboardEventKeyMotion == Pressed
            then let code = SDL.keysymKeycode keyboardEventKeysym
                 in code `elem` [SDL.KeycodeQ, SDL.KeycodeEscape]
            else False
        WindowClosedEvent WindowClosedEventData{} -> True
        _ -> False

data MyShader = MyShader
  { msShaderProgram :: GLU.ShaderProgram
  , msPosLoc        :: GL.AttribLocation
  , msTextureLoc    :: GL.UniformLocation
  }

makeMyShader :: IO MyShader
makeMyShader = do
  sp <- GLU.simpleShaderProgram "_data/tex.vert" "_data/tex.frag"
  let posLoc = GLU.getAttrib sp "Position"
      texLoc = GLU.getUniform sp "Texture"
  return $ MyShader sp posLoc texLoc

data MyResource = MyResource
  { mrVtxBuf  :: GL.BufferObject
  , mrElmBuf  :: GL.BufferObject
  , mrTexture :: GL.TextureObject
  }

makeMyResource :: IO MyResource
makeMyResource =
  MyResource
    <$> GLU.makeBuffer GL.ArrayBuffer vertexBufferData
    <*> GLU.makeBuffer GL.ElementArrayBuffer [0..3::GL.GLuint]
    <*> (fromRight <$> KG.readTexture "_data/in_transit.png")
  where
    vertexBufferData :: [GL.GLfloat]
    vertexBufferData = [-0.8, -0.8, 0.8, -0.8, -0.8, 0.8, 0.8, 0.8]

ready :: MyShader -> MyResource -> IO ()
ready MyShader{..} MyResource{..} = do
  GL.textureFilter   GL.Texture2D   $= ((GL.Nearest, Nothing), GL.Nearest)
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Mirrored, GL.ClampToEdge)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Mirrored, GL.ClampToEdge)
  --
  GL.clear [GL.ColorBuffer]
  GL.currentProgram $= Just (GLU.program msShaderProgram)
  -- Texture
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just mrTexture
  GL.uniform msTextureLoc $= GL.Index1 (0 :: GL.GLint)
  --
  GL.bindBuffer GL.ArrayBuffer $= Just mrVtxBuf
  let stride =  fromIntegral $ sizeOf (undefined :: GL.GLfloat) * 2
      vad = GL.VertexArrayDescriptor 2 GL.Float stride GLU.offset0
  GL.vertexAttribPointer msPosLoc $= (GL.ToFloat, vad)
  GL.vertexAttribArray msPosLoc $= GL.Enabled
  GL.bindBuffer GL.ElementArrayBuffer $= Just mrElmBuf
