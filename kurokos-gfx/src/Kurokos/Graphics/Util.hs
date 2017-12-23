module Kurokos.Graphics.Util where

import qualified Control.Exception         as E
import           Control.Monad             (unless)
import           Foreign.C.Types           (CInt)

import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

makeVAO :: IO a -> IO (a, GL.VertexArrayObject)
makeVAO setup = do
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  a <- setup
  GL.bindVertexArrayObject $= Nothing
  return (a, vao)

throwIfNot0 :: String -> IO CInt -> IO ()
throwIfNot0 tag m = do
  r <- m
  unless (r == 0) $
    E.throwIO $ userError $ "throwIfNot0 @" ++ tag ++ ": " ++ show r
