{-# LANGUAGE OverloadedStrings #-}
module Kurokos.Graphics.Shader.Text
  ( TextShader
  , newTextShader
  ) where

import qualified Data.ByteString           as BS
import qualified Data.Vector.Storable      as V
import           Linear.V4

import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Kurokos.Graphics.Util (makeVAO)
import           Kurokos.Graphics.Types
import           Kurokos.Graphics.Shader

data TextShader = TextShader
  { sProgram          :: GL.Program
  -- , sAttrCoord        :: AttribVar TagVec2
  , sAttrTexCoord     :: AttribVar TagVec2
  , sUniformModelView :: UniformVar TagMat4
  , sUniformProj      :: UniformVar TagMat4
  , sColorVar         :: UniformVar TagVec4
  , sUniformTexture   :: UniformVar TagSampler2D
  , sVao              :: GL.VertexArrayObject
  , sTexCoordVbo      :: GL.BufferObject
  }

instance Shader TextShader where
  shdrProgram    = sProgram
  shdrModelView  = sUniformModelView
  shdrProjection = sUniformProj

instance TextureShader TextShader where
  shdrVAO          = sVao
  shdrTexCoordVbo  = sTexCoordVbo
  shdrTexCoordAttr = sAttrTexCoord
  shdrSampler2D    = sUniformTexture

instance ColorShader TextShader where
  shdrColor = sColorVar

newTextShader :: IO TextShader
newTextShader = do
  sp <- GLU.simpleShaderProgramBS vert frag
  let attrCoord = AttribVar TagVec2 $ GLU.getAttrib sp "VertexCoord"
      attrTexCoord = AttribVar TagVec2 $ GLU.getAttrib sp "TexCoord"
      uniformModelView = UniformVar TagMat4 $ GLU.getUniform sp "ModelView"
      uniformProj = UniformVar TagMat4 $ GLU.getUniform sp "Projection"
      uniformColor = UniformVar TagVec4 $ GLU.getUniform sp "BasisColor"
      uniformTexture = UniformVar (TagSampler2D 0) (GLU.getUniform sp "Texture")
  -- * Setup
  setupSampler2D uniformTexture
  (buf,vao) <- makeVAO $ do
          setupVec2 attrCoord $ V.fromList vtxPs
          buf <- setupVec2 attrTexCoord $ V.fromList texPs
          -- Element
          elmBuf <- GLU.makeBuffer GL.ElementArrayBuffer [0..3::GL.GLuint]
          GL.bindBuffer GL.ElementArrayBuffer $= Just elmBuf
          return buf
  -- Initial Uniform
  withProgram (GLU.program sp) $
    setUniformVec4 uniformColor (V4 1 1 1 1)
  return $ TextShader
    (GLU.program sp)
    -- attrCoord
    attrTexCoord
    uniformModelView
    uniformProj
    uniformColor
    uniformTexture
    vao
    buf
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
