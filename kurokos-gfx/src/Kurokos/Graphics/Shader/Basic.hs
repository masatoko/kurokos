{-# LANGUAGE RecordWildCards #-}
module Kurokos.Graphics.Shader.Basic where

import           Data.Maybe                (fromMaybe)
import           Foreign.C.Types           (CInt)
import           Foreign.Storable          (sizeOf)
import           Linear

import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (get, ($=))
import qualified Graphics.Rendering.OpenGL as GL

import qualified Kurokos.Graphics.Camera   as Cam
import           Kurokos.Graphics.Texture  (Texture (..))
import           Kurokos.Graphics.Types

import Kurokos.Graphics.Shader

data BasicShader = BasicShader
  { brProgram      :: GL.Program
  , brCoordVar     :: AttribVar TagVec2
  , brTexCoordVar  :: AttribVar TagVec2
  , brModelViewVar :: UniformVar TagMat4
  , brProjVar      :: UniformVar TagMat4
  , brTexVar       :: UniformVar TagSampler2D
  , brVao          :: GLU.VAO
  }

renderTexByBasicRenderer_ :: BasicShader -> RContext -> Texture -> IO ()
renderTexByBasicRenderer_ r =
  renderTexByBasicRenderer r Cam.mkCamera

renderTexByBasicRenderer :: BasicShader -> Cam.Camera -> RContext -> Texture -> IO ()
renderTexByBasicRenderer BasicShader{..} cam rctx (Texture tex texW texH) =
  withProgram brProgram $ do
    setUniformMat4 brModelViewVar (view !*! model)
    setUniformSampler2D brTexVar tex
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.blend $= GL.Enabled
    GLU.withVAO brVao $
      GL.drawElements GL.TriangleStrip 4 GL.UnsignedInt GLU.offset0
    GL.blend $= GL.Disabled
  where
    texW' = fromIntegral texW
    texH' = fromIntegral texH
    RContext (V2 x y) mSize mRad mRotCenter = rctx
    V2 sizeX sizeY = fromMaybe (V2 texW' texH') mSize
    V2 rotX0 rotY0 = fromMaybe (V2 (sizeX / 2) (sizeY / 2)) mRotCenter

    view = Cam.viewMatFromCam cam
    model =
      trans !*! rot !*! scaleMat
      where
        trans = mkTransformationMat identity $ V3 x y 0
        rot = case mRad of
                Nothing  -> identity
                Just rad -> let
                  rot = m33_to_m44 . fromQuaternion . axisAngle (V3 0 0 1) $ rad
                  in back !*! rot !*! go

        k = V3 rotX0 rotY0 0
        go = mkTransformationMat identity (k ^* (-1))
        back = mkTransformationMat identity k

        scaleMat = V4 (V4 sizeX 0 0 0)
                      (V4 0 sizeY 0 0)
                      (V4 0 0     1 0)
                      (V4 0 0     0 1)

-- | Update projection matrix of BasicShader
updateBasicRenderer :: ProjectionType -> V2 CInt -> BasicShader -> IO ()
updateBasicRenderer ptype (V2 winW winH) BasicShader{..} =
  withProgram brProgram $
    setUniformMat4 brProjVar $ projMat ptype
  where
    w = fromIntegral winW
    h = fromIntegral winH

    projMat Ortho              = ortho 0 w 0 h 1 (-1)
    projMat (Frustum near far) = frustum 0 w 0 h near far

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

    setupVec2 :: AttribVar TagVec2 -> [GL.GLfloat] -> IO ()
    setupVec2 (AttribVar TagVec2 loc) ps = do
      buf <- GLU.makeBuffer GL.ArrayBuffer ps
      GL.bindBuffer GL.ArrayBuffer $= Just buf
      GL.vertexAttribPointer loc $= (GL.ToFloat, vad)
      GL.vertexAttribArray loc $= GL.Enabled
      where
        stride =  fromIntegral $ sizeOf (undefined :: GL.GLfloat) * 2
        vad = GL.VertexArrayDescriptor 2 GL.Float stride GLU.offset0

    setupSampler2D :: UniformVar TagSampler2D -> IO ()
    setupSampler2D (UniformVar (TagSampler2D num) loc) =
      GL.activeTexture $= GL.TextureUnit num
