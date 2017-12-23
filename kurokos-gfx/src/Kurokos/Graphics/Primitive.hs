{-# LANGUAGE RecordWildCards #-}
module Kurokos.Graphics.Primitive where

import           Data.Foldable                     (foldr')
import qualified Data.Vector.Storable              as V
import qualified Graphics.GLUtil                   as GLU
import           Graphics.Rendering.OpenGL         (($=))
import qualified Graphics.Rendering.OpenGL         as GL

import qualified Kurokos.Graphics.Camera           as Cam
import           Kurokos.Graphics.Render           (mkModelView, setModelView)
import           Kurokos.Graphics.Shader
import           Kurokos.Graphics.Shader.Primitive
import           Kurokos.Graphics.Types            (Color)
import           Kurokos.Graphics.Util             (makeVAO)
import           Kurokos.Graphics.Vect
import           Kurokos.Renderer                  (Renderer (rndrPrimShader))

data Prim = Prim
  { primVAO   :: GL.VertexArrayObject
  , primVBO   :: GL.BufferObject
  , primMode  :: GL.PrimitiveMode
  , primCount :: GL.NumArrayIndices
  }

-- Render

drawPrim :: Renderer -> V2 Int -> Prim -> IO ()
drawPrim rndr pos Prim{..} = do
  setModelView (rndrPrimShader rndr) mv
  withShaderProgram rndr $ do
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.blend $= GL.Enabled
    GLU.withVAO primVAO $
      GL.drawArrays primMode 0 primCount
    GL.blend $= GL.Disabled
  where
    mv = mkModelView Cam.camForVertFlip pos

setPrimColor :: Renderer -> Color -> IO ()
setPrimColor rndr = setColor (rndrPrimShader rndr)

-- Make

freePrim :: Prim -> IO ()
freePrim prim = do
  GLU.deleteVAO . primVAO $ prim
  GL.deleteObjectName . primVBO $ prim

newPrim :: Foldable t => Renderer -> GL.PrimitiveMode -> t (Point V2 Float) -> IO Prim
newPrim rndr pmode v0 = do
  (buf, vao) <- makeVAO $ setupVec2 attrCoord v'
  return $ Prim vao buf pmode numArrayIndices
  where
    attrCoord = sAttrCoord $ rndrPrimShader rndr
    numArrayIndices = fromIntegral $ V.length v' `div` 2
    v' = foldr' work V.empty v0
      where
        work (P (V2 x y)) v = x `V.cons` (y `V.cons` v)

newRectangle :: Renderer -> V2 Float -> IO Prim
newRectangle rndr (V2 w h) = do
  (buf, vao) <- makeVAO $ setupVec2 attrCoord v
  return $ Prim vao buf GL.LineLoop 4
  where
    attrCoord = sAttrCoord $ rndrPrimShader rndr
    v = V.fromList [0, 0, w, 0, w, h, 0, h]

newFillRectangle :: Renderer -> V2 Float -> IO Prim
newFillRectangle rndr (V2 w h) = do
  (buf, vao) <- makeVAO $ setupVec2 attrCoord v
  return $ Prim vao buf GL.TriangleStrip 4
  where
    attrCoord = sAttrCoord $ rndrPrimShader rndr
    v = V.fromList [0, 0, w, 0, 0, h, w, h]

-- Internal Helper

withShaderProgram :: Renderer -> IO a -> IO a
withShaderProgram rndr = withProgram (shdrProgram (rndrPrimShader rndr))
