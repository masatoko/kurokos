module Kurokos.Graphics.Render
  ( renderByShader_
  , renderByShader
  ) where

import           Data.Maybe                (fromMaybe)
import           Linear

import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import qualified Kurokos.Graphics.Camera   as Cam
import           Kurokos.Graphics.Shader


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
                  rot' = m33_to_m44 . fromQuaternion . axisAngle (V3 0 0 1) $ rad
                  in back !*! rot' !*! go

        k = V3 rotX0 rotY0 0
        go = mkTransformationMat identity (k ^* (-1))
        back = mkTransformationMat identity k

        scaleMat = V4 (V4 sizeX 0 0 0)
                      (V4 0 sizeY 0 0)
                      (V4 0 0     1 0)
                      (V4 0 0     0 1)