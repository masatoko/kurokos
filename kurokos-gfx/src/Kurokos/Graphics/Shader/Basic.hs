{-# LANGUAGE OverloadedStrings #-}
module Kurokos.Graphics.Shader.Basic
  ( BasicShader
  , newBasicShader
  , setTexCoordVbo
  , setRenderArea
  ) where

import qualified Data.ByteString           as BS
import qualified Data.Vector.Storable      as V
import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import           Linear.V2                 (V2 (..))

import           Kurokos.Graphics.Shader
import           Kurokos.Graphics.Types
import           Kurokos.Graphics.Util     (makeVAO)

data BasicShader = BasicShader
  { sProgram             :: GL.Program
  , sAttrTexCoord        :: AttribVar TagVec2
  , sUniformModelView    :: UniformVar TagMat4
  , sUniformProj         :: UniformVar TagMat4
  , sUniformRenderAreaLB :: UniformVar TagVec2
  , sUniformRenderAreaRT :: UniformVar TagVec2
  , sUniformTexture      :: UniformVar TagSampler2D
  , sVao                 :: GL.VertexArrayObject
  , sTexVbo              :: TypedBufferObject TagVec2
  }

instance Shader BasicShader where
  shdrProgram    = sProgram
  shdrModelView  = sUniformModelView
  shdrProjection = sUniformProj

instance TextureShader BasicShader where
  shdrVAO          = sVao
  shdrTexCoordVbo  = sTexVbo
  shdrTexCoordAttr = sAttrTexCoord
  shdrSampler2D    = sUniformTexture

newBasicShader :: IO BasicShader
newBasicShader = do
  sp <- GLU.simpleShaderProgramBS vert frag
  let attrCoord = AttribVar TagVec2 $ GLU.getAttrib sp "VertexCoord"
      attrTexCoord = AttribVar TagVec2 $ GLU.getAttrib sp "TexCoord"
  (tbo,vao) <- makeVAO $ do
          setupVec2 attrCoord $ V.fromList vtxPs
          tbo <- setupVec2 attrTexCoord $ V.fromList texPs
          -- Element
          elmBuf <- GLU.makeBuffer GL.ElementArrayBuffer [0..3::GL.GLuint]
          GL.bindBuffer GL.ElementArrayBuffer $= Just elmBuf
          return tbo
  return $ BasicShader
    (GLU.program sp)
    attrTexCoord
    (UniformVar TagMat4 $ GLU.getUniform sp "ModelView")
    (UniformVar TagMat4 $ GLU.getUniform sp "Projection")
    (UniformVar TagVec2 $ GLU.getUniform sp "RenderAreaLB")
    (UniformVar TagVec2 $ GLU.getUniform sp "RenderAreaRT")
    (UniformVar (TagSampler2D (GL.TextureUnit 0)) (GLU.getUniform sp "Texture"))
    vao
    tbo
  where
    vtxPs :: [GL.GLfloat]
    vtxPs = [0, 0, 1, 0, 0, 1, 1, 1]

    texPs :: [GL.GLfloat]
    texPs = [0, 0, 1, 0, 0, 1, 1, 1]

    -- texPs :: [GL.GLfloat]
    -- texPs = [0, 1, 1, 1, 0, 0, 1, 0]

setTexCoordVbo :: BasicShader -> TypedBufferObject TagVec2 -> IO ()
setTexCoordVbo shdr (TBO buf) =
  GLU.withVAO (shdrVAO shdr) $ do
    GL.bindBuffer GL.ArrayBuffer $= Just buf
    GL.vertexAttribPointer loc $= (GL.ToFloat, vad)
    GL.vertexAttribArray loc $= GL.Enabled
  where
    AttribVar TagVec2 loc = shdrTexCoordAttr shdr
    vad = GL.VertexArrayDescriptor 2 GL.Float 0 GLU.offset0

setRenderArea :: BasicShader
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
  , "in vec2 TexCoord;"
  , "out vec4 BiasCoord;"
  , "out vec2 OTexCoord;"
  , ""
  , "uniform mat4 Projection;"
  , "uniform mat4 ModelView;"
  -- , "const mat4 Bias = mat4(0.5,0.0,0.0,0.5, 0.0,0.5,0.0,0.5, 0.0,0.0,0.5,0.5, 0.0,0.0,0.0,1);"
  , ""
  , "void main()"
  , "{"
  , "  gl_Position = Projection * ModelView * vec4( VertexCoord, 0, 1 );"
  , "  BiasCoord = vec4( gl_Position.xyz * 0.5 + vec3(0.5), 1 );"
  , "  OTexCoord = TexCoord;"
  , "}"
  ]

frag :: BS.ByteString
frag = BS.intercalate "\n"
  [ "#version 400"
  , ""
  , "uniform sampler2D Texture;"
  , "in vec2 OTexCoord;"
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
  , "    FragColor = texture( Texture, OTexCoord );"
  , "  } else {"
  , "    discard;"
  , "  }"
  , "}"
  ]
