{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kurokos.Graphics.Shader.Text
  ( TextShader
  , newTextShader
  , setColor
  ) where

import qualified Data.ByteString           as BS
import           Data.Word                 (Word8)
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

newTextShader :: IO TextShader
newTextShader = do
  sp <- GLU.simpleShaderProgramBS vert frag
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

setColor :: TextShader -> V3 Word8 -> IO ()
setColor TextShader{..} color =
  withProgram sProgram $
    setUniformVec3 sColorVar color'
  where
    color' = (/ 255) . fromIntegral <$> color

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
  , "out vec4 FragColor;"
  , ""
  , "void main()"
  , "{"
  , "  FragColor = texture2D( Texture, OTexCoord );"
  , "}"
  ]
