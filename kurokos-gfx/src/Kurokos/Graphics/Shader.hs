{-# LANGUAGE RecordWildCards #-}
module Kurokos.Graphics.Shader where

import           Foreign.Storable          (sizeOf)

import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (($=), get)
import qualified Graphics.Rendering.OpenGL as GL

import           Kurokos.Graphics.Types

data BasicTextureShader = BasicTextureShader
  { btsProgram  :: GL.Program
  , btsCoordVar :: AttribVar TagVec2
  , btsTexVar   :: UniformVar TagSampler2D
  }

newBasicShaderProgram :: IO BasicTextureShader
newBasicShaderProgram = do
  sp <- GLU.simpleShaderProgram "_data/tex.vert" "_data/tex.frag"
  return $ BasicTextureShader
    (GLU.program sp)
    (AttribVar TagVec2 $ GLU.getAttrib sp "VertexCoord")
    (UniformVar (TagSampler2D 0) (GLU.getUniform sp "Texture"))

-- | Renderable texture
data RTexture = RTexture BasicTextureShader GLU.VAO GL.TextureObject

renderRTexture :: RTexture -> IO ()
renderRTexture (RTexture BasicTextureShader{..} vao tex) =
  withProgram btsProgram $ do
    bindSampler2D btsTexVar tex
    GLU.withVAO vao $
      GL.drawElements GL.TriangleStrip 4 GL.UnsignedInt GLU.offset0

makeBasicRTexture :: BasicTextureShader -> GL.TextureObject -> IO RTexture
makeBasicRTexture bts tex = do
  setupSampler2D (btsTexVar bts) tex
  vao <- GLU.makeVAO $ setupVec2 $ btsCoordVar bts
  return $ RTexture bts vao tex
  where
    ps :: [GL.GLfloat]
    ps = [-1, -1, 1, -1, -1, 1, 1, 1]

    setupVec2 :: AttribVar TagVec2 -> IO ()
    setupVec2 (AttribVar TagVec2 loc) = do
      buf <- GLU.makeBuffer GL.ArrayBuffer ps
      GL.bindBuffer GL.ArrayBuffer $= Just buf
      GL.vertexAttribPointer loc $= (GL.ToFloat, vad)
      GL.vertexAttribArray loc $= GL.Enabled
      --
      elmBuf <- GLU.makeBuffer GL.ElementArrayBuffer [0..3::GL.GLuint]
      GL.bindBuffer GL.ElementArrayBuffer $= Just elmBuf
      where
        stride =  fromIntegral $ sizeOf (undefined :: GL.GLfloat) * 2
        vad = GL.VertexArrayDescriptor 2 GL.Float stride GLU.offset0

    setupSampler2D :: UniformVar TagSampler2D -> GL.TextureObject -> IO ()
    setupSampler2D (UniformVar (TagSampler2D num) loc) tex =
      GL.activeTexture $= GL.TextureUnit num

bindSampler2D :: UniformVar TagSampler2D -> GL.TextureObject -> IO ()
bindSampler2D (UniformVar (TagSampler2D num) loc) tex = do
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
