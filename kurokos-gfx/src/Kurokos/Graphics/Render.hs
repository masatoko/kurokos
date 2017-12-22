module Kurokos.Graphics.Render
  ( renderByShader_
  , renderByShader
  , renderText
  ) where

import           Data.Maybe                   (fromMaybe)
import qualified Data.Vector                  as V
import           Linear

import qualified Graphics.GLUtil              as GLU
import           Graphics.Rendering.OpenGL    (($=))
import qualified Graphics.Rendering.OpenGL    as GL

import qualified Kurokos.Graphics.Camera      as Cam
import           Kurokos.Graphics.Shader
import           Kurokos.Graphics.Shader.Text (TextShader, setColor)
import           Kurokos.Graphics.Types

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

-- Text

renderText :: V2 Int -> TextShader -> TextTexture -> IO ()
renderText (V2 x0 iy) shdr ts = do
  setColor shdr $ V3 255 0 0
  V.foldM_ renderChar (fromIntegral x0) ts
  where
    y0 = fromIntegral iy

    renderChar x (CharTexture tex left _top dx _ offY) = do
      let x' = x + fromIntegral left
          y' = y0 + fromIntegral offY
          size = fromIntegral <$> V2 (texWidth tex) (texHeight tex)
          ctx' = RContext (V2 x' y') size Nothing Nothing
      setTexture shdr tex
      renderByShader_ shdr ctx'
      return $ x + dx
