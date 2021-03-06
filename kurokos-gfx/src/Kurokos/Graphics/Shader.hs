module Kurokos.Graphics.Shader where

import           Control.Monad.IO.Class        (MonadIO)
import qualified Data.Vector.Storable          as V
-- import           Foreign.C.Types               (CInt)
-- import           Foreign.Storable              (sizeOf)
import           Linear

import qualified Graphics.GLUtil               as GLU
import qualified Graphics.GLUtil.BufferObjects as BO
import           Graphics.Rendering.OpenGL     (get, ($=))
import qualified Graphics.Rendering.OpenGL     as GL

import           Kurokos.Graphics.Types

addUniformLocation :: GL.UniformLocation -> Int -> GL.UniformLocation
addUniformLocation (GL.UniformLocation a) b =
  GL.UniformLocation $ a + fromIntegral b

-- Update Uniform
setUniformBool :: UniformVar TagBool -> Bool -> IO ()
setUniformBool (UniformVar TagBool loc) val =
  GLU.asUniform (unmarshal val) loc
  where
    unmarshal :: Bool -> GL.GLint
    unmarshal True  = 1
    unmarshal False = 0

setUniformInt :: UniformVar TagInt -> GL.GLint -> IO ()
setUniformInt (UniformVar TagInt loc) val =
  GLU.asUniform val loc

setUniformFloat :: UniformVar TagFloat -> GL.GLfloat -> IO ()
setUniformFloat (UniformVar TagFloat loc) val =
  GLU.asUniform val loc

setUniformAryFloat :: UniformVar TagAryFloat -> Int -> GL.GLfloat -> IO ()
setUniformAryFloat (UniformVar TagAryFloat loc) aryIdx val =
  GLU.asUniform val $ addUniformLocation loc aryIdx

setUniformMat4 :: UniformVar TagMat4 -> M44 GL.GLfloat -> IO ()
setUniformMat4 (UniformVar TagMat4 loc) val =
  GLU.asUniform val loc

setUniformAryMat4 :: UniformVar TagAryMat4 -> Int -> M44 GL.GLfloat -> IO ()
setUniformAryMat4 (UniformVar TagAryMat4 loc) aryIdx val =
  GLU.asUniform val $ addUniformLocation loc aryIdx

setUniformMat3 :: UniformVar TagMat3 -> M33 GL.GLfloat -> IO ()
setUniformMat3 (UniformVar TagMat3 loc) val =
  GLU.asUniform val loc

setUniformVec2 :: UniformVar TagVec2 -> V2 GL.GLfloat -> IO ()
setUniformVec2 (UniformVar TagVec2 loc) val =
  GLU.asUniform val loc

setUniformVec3 :: UniformVar TagVec3 -> V3 GL.GLfloat -> IO ()
setUniformVec3 (UniformVar TagVec3 loc) val =
  GLU.asUniform val loc

setUniformVec4 :: UniformVar TagVec4 -> V4 GL.GLfloat -> IO ()
setUniformVec4 (UniformVar TagVec4 loc) val =
  GLU.asUniform val loc

setUniformSampler2D :: UniformVar TagSampler2D -> GL.TextureObject -> IO ()
setUniformSampler2D (UniformVar (TagSampler2D texUnit) loc) tex = do
  GL.activeTexture $= texUnit
  GL.textureBinding GL.Texture2D $= Just tex -- glBindTexture
  GLU.asUniform texUnit loc

setUniformSamplerCube :: UniformVar TagSamplerCube -> GL.TextureObject -> IO ()
setUniformSamplerCube (UniformVar (TagSamplerCube texUnit) loc) tex = do
  GL.activeTexture $= texUnit
  GL.textureBinding GL.TextureCubeMap $= Just tex -- glBindTexture
  GLU.asUniform texUnit loc

-- Setup
setupAttribChunkf :: GL.NumComponents -> GL.AttribLocation -> V.Vector GL.GLfloat -> IO (TypedBufferObject tag)
setupAttribChunkf numInChunk loc ps = do
  buf <- BO.fromVector GL.ArrayBuffer ps
  GL.bindBuffer GL.ArrayBuffer $= Just buf
  GL.vertexAttribPointer loc $= (GL.ToFloat, vad)
  GL.vertexAttribArray loc $= GL.Enabled
  return $ TBO buf
  where
    stride = 0
    vad = GL.VertexArrayDescriptor numInChunk GL.Float stride GLU.offset0

setupAttribChunki :: GL.NumComponents -> GL.AttribLocation -> V.Vector GL.GLint -> IO (TypedBufferObject tag)
setupAttribChunki numInChunk loc ps = do
  buf <- BO.fromVector GL.ArrayBuffer ps
  GL.bindBuffer GL.ArrayBuffer $= Just buf
  GL.vertexAttribPointer loc $= (GL.KeepIntegral, vad)
  GL.vertexAttribArray loc $= GL.Enabled
  return $ TBO buf
  where
    stride = 0
    vad = GL.VertexArrayDescriptor numInChunk GL.Int stride GLU.offset0

setupVec2 :: AttribVar TagVec2 -> V.Vector GL.GLfloat -> IO (TypedBufferObject TagVec2)
setupVec2 (AttribVar TagVec2 loc) = setupAttribChunkf 2 loc

setupVec3 :: AttribVar TagVec3 -> V.Vector GL.GLfloat -> IO (TypedBufferObject TagVec3)
setupVec3 (AttribVar TagVec3 loc) = setupAttribChunkf 3 loc

setupVec4 :: AttribVar TagVec4 -> V.Vector GL.GLfloat -> IO (TypedBufferObject TagVec4)
setupVec4 (AttribVar TagVec4 loc) = setupAttribChunkf 4 loc

setupAttribFloat :: AttribVar TagFloat -> V.Vector GL.GLfloat -> IO (TypedBufferObject TagFloat)
setupAttribFloat (AttribVar TagFloat loc) = setupAttribChunkf 1 loc

setupAttribInt :: AttribVar TagInt -> V.Vector GL.GLint -> IO (TypedBufferObject TagInt)
setupAttribInt (AttribVar TagInt loc) = setupAttribChunki 1 loc

-- Shader class
class Shader a where
  shdrProgram    :: a -> GL.Program
  shdrModelView  :: a -> UniformVar TagMat4
  shdrProjection :: a -> UniformVar TagMat4

class TextureShader a where
  shdrVAO          :: a -> GL.VertexArrayObject
  shdrTexCoordVbo  :: a -> TypedBufferObject TagVec2
  shdrTexCoordAttr :: a -> AttribVar TagVec2
  shdrSampler2D    :: a -> UniformVar TagSampler2D

class ColorShader a where
  shdrColor :: a -> UniformVar TagVec4

-- | Update projection matrix of BasicShader
-- setProjection :: Shader a => a -> ProjectionType -> V2 CInt -> Bool -> IO ()
-- setProjection shdr ptype (V2 winW winH) vertFlip =
--   withProgram (shdrProgram shdr) $
--     setUniformMat4 (shdrProjection shdr) $ projMat ptype
--   where
--     w = fromIntegral winW
--     h = fromIntegral winH
--     projMat Ortho =
--       if vertFlip
--         then ortho 0 w (-h) 0 (-1) 1
--         else ortho 0 w 0    h 1    (-1)
--     projMat (Frustum near far) = frustum 0 w 0 h near far

setProjection :: Shader a => a -> M44 Float -> IO ()
setProjection shdr mat =
  withProgram (shdrProgram shdr) $
    setUniformMat4 (shdrProjection shdr) mat

setTexture :: (Shader a, TextureShader a) => a -> GL.TextureObject -> IO ()
setTexture shdr tex =
  withProgram (shdrProgram shdr) $
    setUniformSampler2D (shdrSampler2D shdr) tex

setColor :: (ColorShader a, Shader a) => a -> V4 Float -> IO ()
setColor shdr color =
  withProgram (shdrProgram shdr) $
    setUniformVec4 (shdrColor shdr) color


-- Util
withProgram :: MonadIO m => GL.Program -> m a -> m a
withProgram p act = do
  cur <- get GL.currentProgram
  if cur == Just p
    then act
    else do
      GL.currentProgram $= Just p
      ret <- act
      GL.currentProgram $= cur
      return ret
