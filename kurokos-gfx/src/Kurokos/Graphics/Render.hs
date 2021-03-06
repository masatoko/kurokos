module Kurokos.Graphics.Render
  ( setModelView
  , mkModelMat
  , mkModelMatForNormalized
  --
  , renderTextureShader
  , renderTextTexture
  , genTextImage_
  ) where

import qualified Control.Exception            as E
import           Control.Monad                (foldM_)
import           Data.Foldable                (toList)
import           Data.Maybe                   (fromMaybe)
import           Linear

import qualified Graphics.GLUtil              as GLU
import           Graphics.Rendering.OpenGL    (get, ($=))
import qualified Graphics.Rendering.OpenGL    as GL

import qualified Kurokos.Graphics.Camera      as Cam
import           Kurokos.Graphics.Matrix      (mkOrtho)
import           Kurokos.Graphics.Shader
import           Kurokos.Graphics.Shader.Text (TextShader)
import           Kurokos.Graphics.Types
import           Kurokos.Graphics.Util        (makeRenderFBO, withFBO)

setModelView :: Shader a => a -> M44 Float -> IO ()
setModelView shdr modelViewMat =
  withProgram (shdrProgram shdr) $
    setUniformMat4 (shdrModelView shdr) modelViewMat

-- TODO: Translation only. Add rotation radius and rotation center. (Should make another RContext?)
mkModelMat :: V2 Int -> M44 Float
mkModelMat pos = model
  where
    V2 x y = fromIntegral <$> pos
    model = trans
      where
        trans = mkTransformationMat identity $ V3 x y 0

-- Specific for 1x1 size VBO.
mkModelMatForNormalized :: RContext -> M44 Float
mkModelMatForNormalized rctx = model
  where
    RContext (V2 x y) (V2 sizeX sizeY) mRad mRotCenter = rctx
    x' = fromIntegral x
    y' = fromIntegral y
    sizeX' = fromIntegral sizeX
    sizeY' = fromIntegral sizeY
    V2 rotX0 rotY0 = fromMaybe (V2 (sizeX' / 2) (sizeY' / 2)) mRotCenter

    model =
      trans !*! rot !*! scaleMat
      where
        trans = mkTransformationMat identity $ V3 x' y' 0
        rot = case mRad of
                Nothing  -> identity
                Just rad -> let
                  rot' = m33_to_m44 . fromQuaternion . axisAngle (V3 0 0 1) $ rad
                  in back !*! rot' !*! go

        k = V3 rotX0 rotY0 0
        go = mkTransformationMat identity (k ^* (-1))
        back = mkTransformationMat identity k

        scaleMat = V4 (V4 sizeX' 0 0 0)
                      (V4 0 sizeY' 0 0)
                      (V4 0 0      1 0)
                      (V4 0 0      0 1)

renderTextureShader :: (TextureShader a, Shader a) => a -> IO ()
renderTextureShader shdr =
  withProgram (shdrProgram shdr) $ do
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    blend0 <- get GL.blend
    GL.blend $= GL.Enabled
    --
    GLU.withVAO (shdrVAO shdr) $
      GL.drawElements GL.TriangleStrip 4 GL.UnsignedInt GLU.offset0
    --
    GL.blend $= blend0

-- Text

renderTextTexture :: Foldable t => TextShader -> M44 Float -> V2 Int -> t CharTexture -> IO ()
renderTextTexture shdr viewMat (V2 x0 y0) =
  foldM_ renderChar x0
  where
    renderChar x (CharTexture tex color fontSize left top dx _) = do
      let x' = x + left
          y' = y0 + fontSize - top
          size = fromIntegral <$> V2 (texWidth tex) (texHeight tex)
          ctx' = RContext (V2 x' y') size Nothing Nothing
      setColor shdr color
      setTexture shdr $ texObject tex
      let mv = viewMat !*! mkModelMatForNormalized ctx'
      setModelView shdr mv
      renderTextureShader shdr
      return $ x + truncate dx

genTextImage_ :: Foldable t => TextShader -> M44 Float -> t CharTexture -> IO Texture
genTextImage_ shdr originalProjMat cs
  | null (toList cs) = E.throwIO $ userError "Empty TextTexture @Kurokos.Graphics.Render.genTextImage_"
  | otherwise        = do
    (fbo, tex) <- makeRenderFBO size
    setProjection shdr projMat
    withFBO fbo size $ do
      GL.clearColor $= GL.Color4 0 0 0 0
      GL.clear [GL.ColorBuffer]
      GL.cullFace $= Nothing
      renderTextTexture shdr viewMat (pure 0) cs
    setProjection shdr originalProjMat -- Set projection matrix back
    return $ Texture tex width height
  where
    projMat = mkOrtho size False
    viewMat = Cam.viewMatFromCam Cam.mkCamera
    --
    size = V2 (fromIntegral width) (fromIntegral height)
    width = ceiling . sum . map _ctAdvanceX . toList $ cs
    height = maximum . map heightOf . toList $ cs
      where
        heightOf ct = _ctFontSize ct - _ctTop ct + texHeight (ctTexture ct)
