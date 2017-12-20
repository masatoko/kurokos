{-# LANGUAGE RecordWildCards #-}
module Kurokos.Graphics.Shader where

import           Foreign.Storable          (sizeOf)

import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (($=), get)
import qualified Graphics.Rendering.OpenGL as GL

import           Kurokos.Graphics.Types

data BasicTextureShader = BasicTextureShader
  { btsProgram  :: GL.Program
  , btsCoordVar :: ShaderVar TagVec2 GL.AttribLocation
  , btsTexVar   :: ShaderVar TagSampler2D GL.UniformLocation
  }

newBasicShaderProgram :: IO BasicTextureShader
newBasicShaderProgram = do
  sp <- GLU.simpleShaderProgram "_data/tex.vert" "_data/tex.frag"
  return $ BasicTextureShader
    (GLU.program sp)
    (ShaderVar TagVec2 $ GLU.getAttrib sp "VertexCoord")
    (ShaderVar (TagSampler2D 0) (GLU.getUniform sp "Texture"))


data MyTexture = MyTexture GLU.VAO GL.TextureObject

renderWith BasicTextureShader{..} (MyTexture vao tex) =
  withProgram btsProgram $ do
    bindSampler2D btsTexVar tex
    GLU.withVAO vao $
      GL.drawElements GL.TriangleStrip 4 GL.UnsignedInt GLU.offset0

makeBasicTexture :: BasicTextureShader -> GL.TextureObject -> IO MyTexture
makeBasicTexture BasicTextureShader{..} tex = do
  setupSampler2D btsTexVar tex
  vao <- GLU.makeVAO $ setupVec2 btsCoordVar
  return $ MyTexture vao tex
  where
    ps :: [GL.GLfloat]
    ps = [-0.3, -0.8, 0.8, -0.8, -0.8, 0.8, 0.8, 0.8]

    setupVec2 :: ShaderVar TagVec2 GL.AttribLocation -> IO ()
    setupVec2 (ShaderVar TagVec2 loc) = do
      buf <- GLU.makeBuffer GL.ArrayBuffer ps
      GL.bindBuffer GL.ArrayBuffer $= Just buf
      let stride =  fromIntegral $ sizeOf (undefined :: GL.GLfloat) * 2
          vad = GL.VertexArrayDescriptor 2 GL.Float stride GLU.offset0
      GL.vertexAttribPointer loc $= (GL.ToFloat, vad)
      GL.vertexAttribArray loc $= GL.Enabled
      --
      elmBuf <- GLU.makeBuffer GL.ElementArrayBuffer [0..3::GL.GLuint]
      GL.bindBuffer GL.ElementArrayBuffer $= Just elmBuf

    setupSampler2D :: ShaderVar TagSampler2D GL.UniformLocation -> GL.TextureObject -> IO ()
    setupSampler2D sv@(ShaderVar (TagSampler2D num) loc) tex =
      GL.activeTexture $= GL.TextureUnit num

bindSampler2D (ShaderVar (TagSampler2D num) loc) tex = do
  GL.textureBinding GL.Texture2D $= Just tex -- glBindTexture
  GLU.asUniform (GL.TextureUnit num) loc

withProgram :: GL.Program -> IO a -> IO a
withProgram p act = do
  cur <- get GL.currentProgram
  if cur == Just p
    then act
    else do
      GL.currentProgram $= Just p
      ret <- act
      GL.currentProgram $= cur
      return ret
