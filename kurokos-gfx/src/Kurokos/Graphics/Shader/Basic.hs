{-# LANGUAGE OverloadedStrings #-}
module Kurokos.Graphics.Shader.Basic
  ( BasicShader
  , newBasicShader
  , setTexCoordVbo
  ) where

import qualified Data.ByteString           as BS
import qualified Data.Vector.Storable      as V
import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Kurokos.Graphics.Util (makeVAO)
import           Kurokos.Graphics.Shader
import           Kurokos.Graphics.Types

data BasicShader = BasicShader
  { sProgram          :: GL.Program
  -- , sAttrCoord        :: AttribVar TagVec2
  , sAttrTexCoord     :: AttribVar TagVec2
  , sUniformModelView :: UniformVar TagMat4
  , sUniformProj      :: UniformVar TagMat4
  , sUniformTexture   :: UniformVar TagSampler2D
  , sVao              :: GL.VertexArrayObject
  , sTexVbo           :: TypedBufferObject TagVec2
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
      uniformModelView = UniformVar TagMat4 $ GLU.getUniform sp "ModelView"
      uniformProj = UniformVar TagMat4 $ GLU.getUniform sp "Projection"
      uniformTexture = UniformVar (TagSampler2D (GL.TextureUnit 0)) (GLU.getUniform sp "Texture")
  -- * Setup
  setupSampler2D uniformTexture
  (tbo,vao) <- makeVAO $ do
          setupVec2 attrCoord $ V.fromList vtxPs
          tbo <- setupVec2 attrTexCoord $ V.fromList texPs
          -- Element
          elmBuf <- GLU.makeBuffer GL.ElementArrayBuffer [0..3::GL.GLuint]
          GL.bindBuffer GL.ElementArrayBuffer $= Just elmBuf
          return tbo
  return $ BasicShader
    (GLU.program sp)
    -- attrCoord
    attrTexCoord
    uniformModelView
    uniformProj
    uniformTexture
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

vert :: BS.ByteString
vert = BS.intercalate "\n"
  [ "#version 130"
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
  [ "#version 130"
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
