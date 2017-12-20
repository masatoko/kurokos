{-# LANGUAGE RecordWildCards #-}
module Kurokos.Graphics.Shader where

import           Foreign.Storable          (sizeOf)
import Linear

import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (($=), get)
import qualified Graphics.Rendering.OpenGL as GL

import           Kurokos.Graphics.Types

-- class IsProgram a where
--   programOf :: a -> GL.Program

data BasicTextureShader = BasicTextureShader
  { btsProgram  :: GL.Program
  , btsCoordVar :: AttribVar TagVec2
  , btsTexCoordVar :: AttribVar TagVec2
  , btsMVPVar   :: UniformVar TagMat4
  , btsTexVar   :: UniformVar TagSampler2D
  }

-- instance IsProgram BasicTextureShader where
--   programOf = btsProgram

newBasicShaderProgram :: IO BasicTextureShader
newBasicShaderProgram = do
  sp <- GLU.simpleShaderProgram "_data/tex.vert" "_data/tex.frag"
  return $ BasicTextureShader
    (GLU.program sp)
    (AttribVar TagVec2 $ GLU.getAttrib sp "VertexCoord")
    (AttribVar TagVec2 $ GLU.getAttrib sp "TexCoord")
    (UniformVar TagMat4 $ GLU.getUniform sp "MVP")
    (UniformVar (TagSampler2D 0) (GLU.getUniform sp "Texture"))

-- | Renderable texture
data RTexture = RTexture BasicTextureShader GLU.VAO GL.TextureObject

renderRTexture :: Float -> RTexture -> IO ()
renderRTexture a (RTexture BasicTextureShader{..} vao tex) =
  withProgram btsProgram $ do
    setUniformMat4 btsMVPVar mat
    setUniformSampler2D btsTexVar tex
    GLU.withVAO vao $
      GL.drawElements GL.TriangleStrip 4 GL.UnsignedInt GLU.offset0
  where
    mat = transpose $ projection * view * model
      where
        projection =
          ortho 0 640 0 480 (-1) 1

        view = lookAt eye center up
          where
            eye = V3 0 0 (-1)
            center = V3 0 0 1 
            up = V3 0 1 0

        model = mkTransformation rot (V3 0 0 0)
          where
            rot = Quaternion 0 (V3 0 0 1)

makeBasicRTexture :: BasicTextureShader -> GL.TextureObject -> IO RTexture
makeBasicRTexture bts tex = do
  setupSampler2D (btsTexVar bts) tex
  vao <- GLU.makeVAO $ do
          setupVec2 (btsCoordVar bts) vtxPs
          setupVec2 (btsTexCoordVar bts) texPs
          -- Element
          elmBuf <- GLU.makeBuffer GL.ElementArrayBuffer [0..3::GL.GLuint]
          GL.bindBuffer GL.ElementArrayBuffer $= Just elmBuf
  return $ RTexture bts vao tex
  where
    w = 48
    h = 48

    vtxPs :: [GL.GLfloat]
    vtxPs = [0, 0, w, 0, 0, h, w, h]

    texPs :: [GL.GLfloat]
    texPs = [0, 0, 1, 0, 0, 1, 1, 1]

    setupVec2 :: AttribVar TagVec2 -> [GL.GLfloat] -> IO ()
    setupVec2 (AttribVar TagVec2 loc) ps = do
      buf <- GLU.makeBuffer GL.ArrayBuffer ps
      GL.bindBuffer GL.ArrayBuffer $= Just buf
      GL.vertexAttribPointer loc $= (GL.ToFloat, vad)
      GL.vertexAttribArray loc $= GL.Enabled
      where
        stride =  fromIntegral $ sizeOf (undefined :: GL.GLfloat) * 2
        vad = GL.VertexArrayDescriptor 2 GL.Float stride GLU.offset0

    setupSampler2D :: UniformVar TagSampler2D -> GL.TextureObject -> IO ()
    setupSampler2D (UniformVar (TagSampler2D num) loc) tex =
      GL.activeTexture $= GL.TextureUnit num

setUniformMat4 :: UniformVar TagMat4 -> M44 GL.GLfloat -> IO ()
setUniformMat4 (UniformVar TagMat4 loc) mat =
  GLU.asUniform mat loc

setUniformSampler2D :: UniformVar TagSampler2D -> GL.TextureObject -> IO ()
setUniformSampler2D (UniformVar (TagSampler2D num) loc) tex = do
  GL.textureBinding GL.Texture2D $= Just tex -- glBindTexture
  GLU.asUniform (GL.TextureUnit num) loc -- TODO: Move to setup

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
