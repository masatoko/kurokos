{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kurokos.Graphics.Shader.Text
  ( TextShader
  , newTextShader
  , setColor
  ) where

import qualified Data.ByteString           as BS
import qualified Data.Vector.Storable      as V
import           Linear

import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Kurokos.Graphics.Types

import           Kurokos.Graphics.Shader

data TextShader = TextShader
  { sProgram      :: GL.Program
  , sCoordVar     :: AttribVar TagVec2
  , sTexCoordVar  :: AttribVar TagVec2
  , sModelViewVar :: UniformVar TagMat4
  , sProjVar      :: UniformVar TagMat4
  , sColorVar     :: UniformVar TagVec4
  , sTexVar       :: UniformVar TagSampler2D
  , sVao          :: GL.VertexArrayObject
  }

instance Shader TextShader where
  shdrProgram    = sProgram
  shdrModelView  = sModelViewVar
  shdrProjection = sProjVar

instance TextureShader TextShader where
  shdrVAO       = sVao
  shdrSampler2D = sTexVar

instance ColorShader TextShader where
  shdrColor = sColorVar

newTextShader :: IO TextShader
newTextShader = do
  sp <- GLU.simpleShaderProgramBS vert frag
  let vtxCoordVar = AttribVar TagVec2 $ GLU.getAttrib sp "VertexCoord"
      texCoordVar = AttribVar TagVec2 $ GLU.getAttrib sp "TexCoord"
      modelViewUniform = UniformVar TagMat4 $ GLU.getUniform sp "ModelView"
      projUniform = UniformVar TagMat4 $ GLU.getUniform sp "Projection"
      colorUniform = UniformVar TagVec4 $ GLU.getUniform sp "BasisColor"
      texUniform = UniformVar (TagSampler2D 0) (GLU.getUniform sp "Texture")
  -- * Setup
  setupSampler2D texUniform
  vao <- GLU.makeVAO $ do
          setupVec2 vtxCoordVar $ V.fromList vtxPs
          setupVec2 texCoordVar $ V.fromList texPs
          -- Element
          elmBuf <- GLU.makeBuffer GL.ElementArrayBuffer [0..3::GL.GLuint]
          GL.bindBuffer GL.ElementArrayBuffer $= Just elmBuf
  -- Initial Uniform
  withProgram (GLU.program sp) $
    setUniformVec4 colorUniform (V4 1 1 1 1)
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
    texPs = [0, 0, 1, 0, 0, 1, 1, 1]

    -- texPs :: [GL.GLfloat]
    -- texPs = [0, 1, 1, 1, 0, 0, 1, 0]

vert :: BS.ByteString
vert = BS.intercalate "\n"
  [ "#version 400"
  , ""
  , "in vec2 VertexCoord;"
  , "in vec2 TexCoord;"
  , "out vec2 OTexCoord;"
  , ""
  , "uniform mat4 Projection;"
  , "uniform mat4 ModelView;"
  , ""
  , "void main()"
  , "{"
  , "  gl_Position = Projection * ModelView * vec4( VertexCoord, 0, 1 );"
  , "  OTexCoord = TexCoord;"
  , "}"
  ]

frag :: BS.ByteString
frag = BS.intercalate "\n"
  [ "#version 400"
  , ""
  , "uniform sampler2D Texture;"
  , "in vec2 OTexCoord;"
  , ""
  , "uniform vec4 BasisColor;"
  , ""
  , "out vec4 FragColor;"
  , ""
  , "void main()"
  , "{"
  , "  float alpha = BasisColor.w * texture2D( Texture, OTexCoord ).a;"
  , "  FragColor = vec4(BasisColor.xyz, alpha);"
  , "}"
  ]
