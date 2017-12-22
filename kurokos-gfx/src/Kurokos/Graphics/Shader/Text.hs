{-# LANGUAGE RecordWildCards #-}
module Kurokos.Graphics.Shader.Text where

import           Data.Maybe                (fromMaybe)
import           Data.Word                 (Word8)
import           Foreign.C.Types           (CInt)
import           Foreign.Storable          (sizeOf)
import           Linear

import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (get, ($=))
import qualified Graphics.Rendering.OpenGL as GL

import qualified Kurokos.Graphics.Camera   as Cam
import           Kurokos.Graphics.Texture  (Texture (..))
import           Kurokos.Graphics.Types

import           Kurokos.Graphics.Shader

data TextShader = TextShader
  { sProgram      :: GL.Program
  , sCoordVar     :: AttribVar TagVec2
  , sTexCoordVar  :: AttribVar TagVec2
  , sModelViewVar :: UniformVar TagMat4
  , sProjVar      :: UniformVar TagMat4
  , sColorVar     :: UniformVar TagVec3
  , sTexVar       :: UniformVar TagSampler2D
  , sVao          :: GL.VertexArrayObject
  }

instance Shader TextShader where
  shdrProgram    = sProgram
  shdrModelView  = sModelViewVar
  shdrProjection = sProjVar
  shdrVAO        = sVao

instance TextureShader TextShader where
  shdrSampler2D = sTexVar

setColor :: TextShader -> V3 Word8 -> IO ()
setColor TextShader{..} color =
  withProgram sProgram $
    setUniformVec3 sColorVar color'
  where
    color' = (/ 255) . fromIntegral <$> color

newTextShader :: IO TextShader
newTextShader = do
  sp <- GLU.simpleShaderProgram "_data/basic-text.vert" "_data/basic-text.frag"
  let vtxCoordVar = AttribVar TagVec2 $ GLU.getAttrib sp "VertexCoord"
      texCoordVar = AttribVar TagVec2 $ GLU.getAttrib sp "TexCoord"
      modelViewUniform = UniformVar TagMat4 $ GLU.getUniform sp "ModelView"
      projUniform = UniformVar TagMat4 $ GLU.getUniform sp "Projection"
      colorUniform = UniformVar TagVec3 $ GLU.getUniform sp "BasisColor"
      texUniform = UniformVar (TagSampler2D 0) (GLU.getUniform sp "Texture")
  -- * Setup
  setupSampler2D texUniform
  vao <- GLU.makeVAO $ do
          setupVec2 vtxCoordVar vtxPs
          setupVec2 texCoordVar texPs
          -- Element
          elmBuf <- GLU.makeBuffer GL.ElementArrayBuffer [0..3::GL.GLuint]
          GL.bindBuffer GL.ElementArrayBuffer $= Just elmBuf
  -- Initial Uniform
  withProgram (GLU.program sp) $
    setUniformVec3 colorUniform (V3 1 1 1)
  return $ TextShader
    (GLU.program sp)
    vtxCoordVar
    texCoordVar
    modelViewUniform
    projUniform
    colorUniform
    texUniform
    vao
  where
    vtxPs :: [GL.GLfloat]
    vtxPs = [0, 0, 1, 0, 0, 1, 1, 1]

    texPs :: [GL.GLfloat]
    texPs = [0, 1, 1, 1, 0, 0, 1, 0]
