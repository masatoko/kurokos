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

-- renderTexByBasicShader_ :: TextShader -> RContext -> Texture -> IO ()
-- renderTexByBasicShader_ r = renderTexByBasicShader r Cam.mkCamera
--
-- renderTexByBasicShader :: TextShader -> Cam.Camera -> RContext -> Texture -> IO ()
-- renderTexByBasicShader TextShader{..} cam rctx (Texture tex texW texH) =
--   withProgram sProgram $ do
--     setUniformMat4 sModelViewVar (view !*! model)
--     setUniformSampler2D sTexVar tex
--     GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
--     GL.blend $= GL.Enabled
--     GLU.withVAO sVao $
--       GL.drawElements GL.TriangleStrip 4 GL.UnsignedInt GLU.offset0
--     GL.blend $= GL.Disabled
--   where
--     texW' = fromIntegral texW
--     texH' = fromIntegral texH
--     RContext (V2 x y) mSize mRad mRotCenter = rctx
--     V2 sizeX sizeY = fromMaybe (V2 texW' texH') mSize
--     V2 rotX0 rotY0 = fromMaybe (V2 (sizeX / 2) (sizeY / 2)) mRotCenter
--
--     view = Cam.viewMatFromCam cam
--     model =
--       trans !*! rot !*! scaleMat
--       where
--         trans = mkTransformationMat identity $ V3 x y 0
--         rot = case mRad of
--                 Nothing  -> identity
--                 Just rad -> let
--                   rot = m33_to_m44 . fromQuaternion . axisAngle (V3 0 0 1) $ rad
--                   in back !*! rot !*! go
--
--         k = V3 rotX0 rotY0 0
--         go = mkTransformationMat identity (k ^* (-1))
--         back = mkTransformationMat identity k
--
--         scaleMat = V4 (V4 sizeX 0 0 0)
--                       (V4 0 sizeY 0 0)
--                       (V4 0 0     1 0)
--                       (V4 0 0     0 1)

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
