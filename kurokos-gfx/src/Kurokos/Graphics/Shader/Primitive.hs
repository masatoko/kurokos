{-# LANGUAGE OverloadedStrings #-}
module Kurokos.Graphics.Shader.Primitive where

import qualified Data.ByteString           as BS
import qualified Graphics.GLUtil           as GLU
import qualified Graphics.Rendering.OpenGL as GL
import           Linear.V4

import           Kurokos.Graphics.Shader
import           Kurokos.Graphics.Types

data PrimitiveShader = PrimitiveShader
  { sProgram          :: GL.Program
  , sAttrCoord        :: AttribVar TagVec2
  , sUniformModelView :: UniformVar TagMat4
  , sUniformProj      :: UniformVar TagMat4
  , sUniformColor     :: UniformVar TagVec4
  }

instance Shader PrimitiveShader where
  shdrProgram    = sProgram
  shdrModelView  = sUniformModelView
  shdrProjection = sUniformProj

instance ColorShader PrimitiveShader where
  shdrColor = sUniformColor

newPrimitiveShader :: IO PrimitiveShader
newPrimitiveShader = do
  sp <- GLU.simpleShaderProgramBS vert frag
  let uniformColor = UniformVar TagVec4 $ GLU.getUniform sp "Color"
  withProgram (GLU.program sp) $
    setUniformVec4 uniformColor (V4 0 1 0 1)
  return $ PrimitiveShader
    (GLU.program sp)
    (AttribVar TagVec2 $ GLU.getAttrib sp "VertexCoord")
    (UniformVar TagMat4 $ GLU.getUniform sp "ModelView")
    (UniformVar TagMat4 $ GLU.getUniform sp "Projection")
    uniformColor

vert :: BS.ByteString
vert = BS.intercalate "\n"
  [ "#version 400"
  , ""
  , "in vec2 VertexCoord;"
  , ""
  , "uniform mat4 Projection;"
  , "uniform mat4 ModelView;"
  , ""
  , "void main()"
  , "{"
  , "  gl_Position = Projection * ModelView * vec4( VertexCoord, 0, 1 );"
  , "}"
  ]

frag :: BS.ByteString
frag = BS.intercalate "\n"
  [ "#version 400"
  , ""
  , "uniform vec4 Color;"
  , ""
  , "out vec4 FragColor;"
  , ""
  , "void main()"
  , "{"
  , "  FragColor = Color;"
  , "}"
  ]
