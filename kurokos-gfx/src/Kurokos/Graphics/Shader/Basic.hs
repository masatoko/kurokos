module Kurokos.Graphics.Shader.Basic where

import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Kurokos.Graphics.Types

import Kurokos.Graphics.Shader

data BasicShader = BasicShader
  { sProgram      :: GL.Program
  , sCoordVar     :: AttribVar TagVec2
  , sTexCoordVar  :: AttribVar TagVec2
  , sModelViewVar :: UniformVar TagMat4
  , sProjVar      :: UniformVar TagMat4
  , sTexVar       :: UniformVar TagSampler2D
  , sVao          :: GL.VertexArrayObject
  }

instance Shader BasicShader where
  shdrProgram    = sProgram
  shdrModelView  = sModelViewVar
  shdrProjection = sProjVar
  shdrVAO        = sVao

instance TextureShader BasicShader where
  shdrSampler2D = sTexVar

newBasicRenderer :: IO BasicShader
newBasicRenderer = do
  sp <- GLU.simpleShaderProgram "_data/basic-texture.vert" "_data/basic-texture.frag"
  let vtxCoordVar = AttribVar TagVec2 $ GLU.getAttrib sp "VertexCoord"
      texCoordVar = AttribVar TagVec2 $ GLU.getAttrib sp "TexCoord"
      modelViewUniform = UniformVar TagMat4 $ GLU.getUniform sp "ModelView"
      projUniform = UniformVar TagMat4 $ GLU.getUniform sp "Projection"
      texUniform = UniformVar (TagSampler2D 0) (GLU.getUniform sp "Texture")
  -- * Setup
  setupSampler2D texUniform
  vao <- GLU.makeVAO $ do
          setupVec2 vtxCoordVar vtxPs
          setupVec2 texCoordVar texPs
          -- Element
          elmBuf <- GLU.makeBuffer GL.ElementArrayBuffer [0..3::GL.GLuint]
          GL.bindBuffer GL.ElementArrayBuffer $= Just elmBuf
  return $ BasicShader
    (GLU.program sp)
    vtxCoordVar
    texCoordVar
    modelViewUniform
    projUniform
    texUniform
    vao
  where
    vtxPs :: [GL.GLfloat]
    vtxPs = [0, 0, 1, 0, 0, 1, 1, 1]

    texPs :: [GL.GLfloat]
    texPs = [0, 1, 1, 1, 0, 0, 1, 0]
