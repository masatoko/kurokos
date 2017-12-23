{-# LANGUAGE RecordWildCards #-}
module Kurokos.Graphics.Primitive where

import           Data.Foldable                     (foldr')
import qualified Data.Vector.Storable              as V
import qualified Graphics.GLUtil                   as GLU
import           Graphics.Rendering.OpenGL         (($=))
import qualified Graphics.Rendering.OpenGL         as GL
import           Linear.V2

import qualified Kurokos.Graphics.Camera           as Cam
import           Kurokos.Graphics.Render           (mkModelView, setModelView)
import           Kurokos.Graphics.Shader
import           Kurokos.Graphics.Shader.Primitive
import           Kurokos.Renderer                  (Renderer (rndrPrimShader))

data Prim = Prim
  { primVAO   :: GL.VertexArrayObject
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

-- Make

newPrim :: Foldable t => Renderer -> GL.PrimitiveMode -> t (V2 Float) -> IO Prim
newPrim rndr pmode v0 = do
  vao <- GLU.makeVAO $ setupVec2 attrCoord v'
  return $ Prim vao pmode numArrayIndices
  where
    attrCoord = sAttrCoord $ rndrPrimShader rndr

    numArrayIndices = fromIntegral $ V.length v' `div` 2
    v' = foldr' work V.empty v0
      where
        work (V2 x y) v = x `V.cons` (y `V.cons` v)

-- Internal Helper

withShaderProgram :: Renderer -> IO a -> IO a
withShaderProgram rndr = withProgram (shdrProgram (rndrPrimShader rndr))
