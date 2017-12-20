{-# LANGUAGE RecordWildCards #-}
module Kurokos.Graphics.Shader where

import           Data.Maybe                (fromMaybe)
import           Foreign.C.Types           (CInt)
import           Foreign.Storable          (sizeOf)
import           Linear

import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (get, ($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Kurokos.Graphics.Texture  (Texture (..))
import           Kurokos.Graphics.Types

-- class IsProgram a where
--   programOf :: a -> GL.Program

data BasicRenderer = BasicRenderer
  { brProgram     :: GL.Program
  , brCoordVar    :: AttribVar TagVec2
  , brTexCoordVar :: AttribVar TagVec2
  , brMVPVar      :: UniformVar TagMat4
  , brTexVar      :: UniformVar TagSampler2D
  , brVao         :: GLU.VAO
  }

-- instance IsProgram BasicRenderer where
--   programOf = brProgram

-- | Rendering context
data RContext = RContext
  { rctxViewSize  :: V2 CInt
  -- ^ View size
  , rctxCoord     :: V2 Float
  -- ^ Left bottom coord
  , rctxSize      :: Maybe (V2 Float)
  -- ^ Size
  , rctxRot       :: Maybe Float
  -- ^ Rotation angle [rad]
  , rctxRotCenter :: Maybe (V2 Float)
  -- ^ Rotation center coord
  }

renderTexByBasicRenderer :: BasicRenderer -> RContext -> Texture -> IO ()
renderTexByBasicRenderer BasicRenderer{..} rctx (Texture tex texW texH) =
  withProgram brProgram $ do
    setUniformMat4 brMVPVar  mvpMat
    setUniformSampler2D brTexVar tex
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.blend $= GL.Enabled
    GLU.withVAO brVao $
      GL.drawElements GL.TriangleStrip 4 GL.UnsignedInt GLU.offset0
    GL.blend $= GL.Disabled
  where
    texW' = fromIntegral texW
    texH' = fromIntegral texH
    RContext (V2 winW winH) (V2 x y) mSize mRad mRotCenter = rctx
    V2 sizeX sizeY = fromMaybe (V2 texW' texH') mSize
    V2 rotX0 rotY0 = fromMaybe (V2 (sizeX / 2) (sizeY / 2)) mRotCenter

    mvpMat = projection !*! model

    projection = ortho 0 (fromIntegral winW) 0 (fromIntegral winH) 1 (-1)

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

newBasicRenderer :: IO BasicRenderer
newBasicRenderer = do
  sp <- GLU.simpleShaderProgram "_data/tex.vert" "_data/tex.frag"
  let vtxCoordVar = AttribVar TagVec2 $ GLU.getAttrib sp "VertexCoord"
      texCoordVar = AttribVar TagVec2 $ GLU.getAttrib sp "TexCoord"
      mvpUniform = UniformVar TagMat4 $ GLU.getUniform sp "MVP"
      texUniform = UniformVar (TagSampler2D 0) (GLU.getUniform sp "Texture")
  -- * Setup
  setupSampler2D texUniform
  vao <- GLU.makeVAO $ do
          setupVec2 vtxCoordVar vtxPs
          setupVec2 texCoordVar texPs
          -- Element
          elmBuf <- GLU.makeBuffer GL.ElementArrayBuffer [0..3::GL.GLuint]
          GL.bindBuffer GL.ElementArrayBuffer $= Just elmBuf
  return $ BasicRenderer
    (GLU.program sp)
    vtxCoordVar
    texCoordVar
    mvpUniform
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

setUniformMat4 :: UniformVar TagMat4 -> M44 GL.GLfloat -> IO ()
setUniformMat4 (UniformVar TagMat4 loc) mat =
  GLU.asUniform mat loc

setUniformSampler2D :: UniformVar TagSampler2D -> GL.TextureObject -> IO ()
setUniformSampler2D (UniformVar (TagSampler2D num) loc) tex = do
  GL.textureBinding GL.Texture2D $= Just tex -- glBindTexture
  GLU.asUniform (GL.TextureUnit num) loc -- TODO: Move to setup

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
