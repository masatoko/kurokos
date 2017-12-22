module Kurokos.Graphics.Shader where

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

-- | Rendering context
data RContext = RContext
  { rctxCoord     :: V2 Float
  -- ^ Left bottom coord
  , rctxSize      :: V2 Float
  -- ^ Size
  , rctxRot       :: Maybe Float
  -- ^ Rotation angle [rad]
  , rctxRotCenter :: Maybe (V2 Float)
  -- ^ Rotation center coord
  }


data ProjectionType
  = Ortho
  | Frustum Float Float -- Near Far
  deriving (Eq, Show)

-- Update Uniform
setUniformMat4 :: UniformVar TagMat4 -> M44 GL.GLfloat -> IO ()
setUniformMat4 (UniformVar TagMat4 loc) mat =
  GLU.asUniform mat loc

setUniformVec3 :: UniformVar TagVec3 -> V3 GL.GLfloat -> IO ()
setUniformVec3 (UniformVar TagVec3 loc) vec =
  GLU.asUniform vec loc

setUniformSampler2D :: UniformVar TagSampler2D -> GL.TextureObject -> IO ()
setUniformSampler2D (UniformVar (TagSampler2D num) loc) tex = do
  GL.textureBinding GL.Texture2D $= Just tex -- glBindTexture
  GLU.asUniform (GL.TextureUnit num) loc -- TODO: Move to setup

-- Setup
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

-- Shader class
class Shader a where
  shdrProgram    :: a -> GL.Program
  shdrModelView  :: a -> UniformVar TagMat4
  shdrProjection :: a -> UniformVar TagMat4
  shdrVAO        :: a -> GL.VertexArrayObject

class TextureShader a where
  shdrSampler2D :: a -> UniformVar TagSampler2D

-- | Update projection matrix of BasicShader
updateProjection :: Shader a => ProjectionType -> V2 CInt -> a -> IO ()
updateProjection ptype (V2 winW winH) shdr =
  withProgram (shdrProgram shdr) $
    setUniformMat4 (shdrProjection shdr) $ projMat ptype
  where
    w = fromIntegral winW
    h = fromIntegral winH

    projMat Ortho              = ortho 0 w 0 h 1 (-1)
    projMat (Frustum near far) = frustum 0 w 0 h near far

setTexture :: (Shader a, TextureShader a) => a -> Texture -> IO ()
setTexture shdr tex =
  withProgram (shdrProgram shdr) $
    setUniformSampler2D (shdrSampler2D shdr) $ texObject tex

renderByShader_ :: Shader a => a -> RContext -> IO ()
renderByShader_ shdr =
  renderByShader shdr Cam.mkCamera

renderByShader :: Shader a => a -> Cam.Camera -> RContext -> IO ()
renderByShader shdr cam rctx =
  withProgram (shdrProgram shdr) $ do
    setUniformMat4 (shdrModelView shdr) (view !*! model)
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.blend $= GL.Enabled
    GLU.withVAO (shdrVAO shdr) $
      GL.drawElements GL.TriangleStrip 4 GL.UnsignedInt GLU.offset0
    GL.blend $= GL.Disabled
  where
    RContext (V2 x y) (V2 sizeX sizeY) mRad mRotCenter = rctx
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

-- Util
withProgram :: GL.Program -> IO a -> IO a
withProgram p act = do
  cur <- get GL.currentProgram
  if cur == Just p
    then act
    else do
      GL.currentProgram $= Just p
      ret <- act
      GL.currentProgram $= cur
      return ret
