module Kurokos.Graphics.Util where

import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

makeVAO :: IO a -> IO (a, GL.VertexArrayObject)
makeVAO setup = do
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  a <- setup
  GL.bindVertexArrayObject $= Nothing
  return (a, vao)
