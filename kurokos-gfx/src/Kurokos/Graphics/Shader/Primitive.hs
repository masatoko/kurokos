{-# LANGUAGE OverloadedStrings #-}
module Kurokos.Graphics.Shader.Primitive where

import qualified Data.ByteString           as BS
import qualified Graphics.GLUtil           as GLU
import qualified Graphics.Rendering.OpenGL as GL
import           Linear.V2
import           Linear.V4

import           Kurokos.Graphics.Shader
import           Kurokos.Graphics.Types

data PrimitiveShader = PrimitiveShader
  { sProgram             :: GL.Program
  , sAttrCoord           :: AttribVar TagVec2
  , sUniformModelView    :: UniformVar TagMat4
  , sUniformProj         :: UniformVar TagMat4
  , sUniformRenderAreaLB :: UniformVar TagVec2
  , sUniformRenderAreaRT :: UniformVar TagVec2
  , sUniformColor        :: UniformVar TagVec4
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
    (UniformVar TagVec2 $ GLU.getUniform sp "RenderAreaLB")
    (UniformVar TagVec2 $ GLU.getUniform sp "RenderAreaRT")
    uniformColor

setRenderArea :: PrimitiveShader
              -> Float -- ^ Left (0 ~ 1)
              -> Float -- ^ Bottom
              -> Float -- ^ Right
              -> Float -- ^ Top
              -> IO ()
setRenderArea shdr x0 y0 x1 y1 =
  withProgram (sProgram shdr) $ do
    setUniformVec2 (sUniformRenderAreaLB shdr) $ V2 x0 y0
    setUniformVec2 (sUniformRenderAreaRT shdr) $ V2 x1 y1

vert :: BS.ByteString
vert = BS.intercalate "\n"
  [ "#version 400"
  , ""
  , "in vec2 VertexCoord;"
  , "out vec4 BiasCoord;"
  , ""
  , "uniform mat4 Projection;"
  , "uniform mat4 ModelView;"
  , ""
  , "void main()"
  , "{"
  , "  gl_Position = Projection * ModelView * vec4( VertexCoord, 0, 1 );"
  , "  BiasCoord = vec4( gl_Position.xyz * 0.5 + vec3(0.5), 1 );"
  , "}"
  ]

frag :: BS.ByteString
frag = BS.intercalate "\n"
  [ "#version 400"
  , ""
  , "uniform vec4 Color;"
  , "in vec4 BiasCoord;"
  , "uniform vec2 RenderAreaLB = vec2(0,0);"
  , "uniform vec2 RenderAreaRT = vec2(1,1);"
  , ""
  , "out vec4 FragColor;"
  , ""
  , "void main()"
  , "{"
  , "  // Is Coord within RenderableArea?"
  , "  vec2 coord = BiasCoord.xy;"
  , "  if (all(lessThanEqual(RenderAreaLB, coord)) && all(lessThanEqual(coord, RenderAreaRT))) {"
  , "    FragColor = Color;"
  , "  } else {"
  , "    discard;"
  , "  }"
  , "}"
  ]
