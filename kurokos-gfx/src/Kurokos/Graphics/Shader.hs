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

data BasicTextureShader = BasicTextureShader
  { btsProgram     :: GL.Program
  , btsCoordVar    :: AttribVar TagVec2
  , btsTexCoordVar :: AttribVar TagVec2
  , btsMVPVar      :: UniformVar TagMat4
  , btsTexVar      :: UniformVar TagSampler2D
  }

-- instance IsProgram BasicTextureShader where
--   programOf = btsProgram

newBasicShaderProgram :: IO BasicTextureShader
newBasicShaderProgram = do
  sp <- GLU.simpleShaderProgram "_data/tex.vert" "_data/tex.frag"
  return $ BasicTextureShader
    (GLU.program sp)
    (AttribVar TagVec2 $ GLU.getAttrib sp "VertexCoord")
    (AttribVar TagVec2 $ GLU.getAttrib sp "TexCoord")
    (UniformVar TagMat4 $ GLU.getUniform sp "MVP")
    (UniformVar (TagSampler2D 0) (GLU.getUniform sp "Texture"))

-- | Renderable texture
data RTexture = RTexture BasicTextureShader GLU.VAO Texture

-- | Rendering context
data RContext = RContext
  { rctxViewSize   :: V2 CInt
  -- ^ View size
  , rctxCoord     :: V2 Float
  -- ^ Left bottom coord
  , rctxScale     :: Maybe (V2 Float)
  -- ^ Scale
  , rctxRot       :: Maybe Float
  -- ^ Rotation angle [rad]
  , rctxRotCenter :: Maybe (V2 Float)
  -- ^ Rotation center coord
  }

renderRTexture :: RContext -> RTexture -> IO ()
renderRTexture rctx (RTexture BasicTextureShader{..} vao (Texture tex texW texH)) =
  withProgram btsProgram $ do
    setUniformMat4 btsMVPVar mvpMat
    setUniformSampler2D btsTexVar tex
    GLU.withVAO vao $
      GL.drawElements GL.TriangleStrip 4 GL.UnsignedInt GLU.offset0
  where
    texW' = fromIntegral texW
    texH' = fromIntegral texH
    RContext (V2 winW winH) (V2 x y) mScale mRad mRotCenter = rctx
    V2 rotX0 rotY0 = fromMaybe (V2 (texW' / 2) (texH' / 2)) mRotCenter

    mvpMat = projection !*! model

    projection = ortho 0 (fromIntegral winW) 0 (fromIntegral winH) 1 (-1)

    -- view = lookAt eye center up
    --   where
    --     eye    = V3 0 0 1
    --     center = V3 0 0 0
    --     up     = V3 0 1 0

    model = case mScale of
              Nothing -> transRot (pure 1)
              Just scale ->
                transRot scale !*! mkScaleMat scale
      where
        mkScaleMat (V2 scaleX scaleY) =
          V4 (V4 scaleX 0 0 0)
             (V4 0 scaleY 0 0)
             (V4 0 0      1 0)
             (V4 0 0      0 1)

        transRot (V2 scaleX scaleY) = trans !*! rot
          where
            trans = mkTransformationMat identity $ V3 x y 0
            rot = case mRad of
                    Nothing  -> identity
                    Just rad -> back !*! mkRot rad !*! go
              where
                mkRot = m33_to_m44 . fromQuaternion . axisAngle (V3 0 0 1)

            k = V3 (scaleX * rotX0) (scaleY * rotY0) 0
            go = mkTransformationMat identity (k ^* (-1))
            back = mkTransformationMat identity k


makeBasicRTexture :: BasicTextureShader -> Texture -> IO RTexture
makeBasicRTexture bts tex@(Texture _ w h) = do
  setupSampler2D (btsTexVar bts)
  vao <- GLU.makeVAO $ do
          setupVec2 (btsCoordVar bts) vtxPs
          setupVec2 (btsTexCoordVar bts) texPs
          -- Element
          elmBuf <- GLU.makeBuffer GL.ElementArrayBuffer [0..3::GL.GLuint]
          GL.bindBuffer GL.ElementArrayBuffer $= Just elmBuf
  return $ RTexture bts vao tex
  where
    w' = fromIntegral w
    h' = fromIntegral h

    vtxPs :: [GL.GLfloat]
    vtxPs = [0, 0, w', 0, 0, h', w', h']

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
