{-# LANGUAGE RecordWildCards #-}
module Kurokos.Graphics.Primitive where

import           Data.Foldable                     (foldr')
import qualified Data.Vector.Storable              as V
import qualified Graphics.GLUtil                   as GLU
import           Graphics.Rendering.OpenGL         (get, ($=))
import qualified Graphics.Rendering.OpenGL         as GL
import           Linear.Matrix

import           Kurokos.Graphics.Render           (mkModelMat, setModelView)
import           Kurokos.Graphics.Shader
import           Kurokos.Graphics.Shader.Primitive
import           Kurokos.Graphics.Types
import           Kurokos.Graphics.Util             (makeVAO)
import           Kurokos.Graphics.Vect
import           Kurokos.Renderer                  (Renderer (..))

data Prim = Prim
  { primVAO   :: GL.VertexArrayObject
  , primVBO   :: TypedBufferObject TagVec2
  , primMode  :: GL.PrimitiveMode
  , primCount :: GL.NumArrayIndices
  } deriving Show

-- Render

drawPrim :: Renderer -> V2 Int -> Prim -> IO ()
drawPrim rndr pos Prim{..} = do
  setModelView (rndrPrimShader rndr) mv
  withShaderProgram rndr $ do
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    blend0 <- get GL.blend
    GL.blend $= GL.Enabled
    --
    GLU.withVAO primVAO $
      GL.drawArrays primMode 0 primCount
    --
    GL.blend $= blend0
  where
    mv = rndrCurrentView rndr !*! mkModelMat pos

setPrimColor :: Renderer -> Color -> IO ()
setPrimColor rndr color =
  setColor (rndrPrimShader rndr) color'
  where
    color' = (/ 255) . fromIntegral <$> color

-- Make

freePrim :: Prim -> IO ()
freePrim prim = do
  GLU.deleteVAO . primVAO $ prim
  GL.deleteObjectName . unTBO . primVBO $ prim

newPrim :: Foldable t => Renderer -> GL.PrimitiveMode -> t (Point V2 Float) -> IO Prim
newPrim rndr pmode v0 = do
  (tbo, vao) <- makeVAO $ setupVec2 attrCoord v'
  return $ Prim vao tbo pmode numArrayIndices
  where
    attrCoord = sAttrCoord $ rndrPrimShader rndr
    numArrayIndices = fromIntegral $ V.length v' `div` 2
    v' = foldr' work V.empty v0
      where
        work (P (V2 x y)) v = x `V.cons` (y `V.cons` v)

newRectangle :: Renderer -> V2 Float -> IO Prim
newRectangle rndr (V2 w h) = do
  (tbo, vao) <- makeVAO $ setupVec2 attrCoord v
  return $ Prim vao tbo GL.LineLoop 4
  where
    attrCoord = sAttrCoord $ rndrPrimShader rndr
    v = V.fromList [0, 0, w, 0, w, h, 0, h]

newFillRectangle :: Renderer -> V2 Float -> IO Prim
newFillRectangle rndr (V2 w h) = do
  (tbo, vao) <- makeVAO $ setupVec2 attrCoord v
  return $ Prim vao tbo GL.TriangleStrip 4
  where
    attrCoord = sAttrCoord $ rndrPrimShader rndr
    v = V.fromList [0, 0, w, 0, 0, h, w, h]

-- Internal Helper

withShaderProgram :: Renderer -> IO a -> IO a
withShaderProgram rndr = withProgram (shdrProgram (rndrPrimShader rndr))
