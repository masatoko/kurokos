{-# LANGUAGE LambdaCase #-}
module Kurokos.Graphics.Texture
  ( Texture (..)
  , readTexture
  , decodeTexture
  , deleteTexture
  ) where

import qualified Codec.Picture             as Pic
import           Codec.Picture.Types       (convertImage)
import qualified Control.Exception         as E
import qualified Data.ByteString           as BS
import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Kurokos.Graphics.Types    (Texture (..))

readTexture :: FilePath -> IO Texture
readTexture fpath =
  decodeTexture =<< BS.readFile fpath

decodeTexture :: BS.ByteString -> IO Texture
decodeTexture bytes =
  case Pic.decodeImage bytes of
    Left err -> E.throwIO $ userError err
    Right dynamicImage -> do
      let image = Pic.convertRGBA8 dynamicImage
          w = Pic.imageWidth image
          h = Pic.imageHeight image
          p = Pic.imageData image
      tex <- GLU.loadTexture $ GLU.texInfo w h GLU.TexRGBA p
      initTexture tex
      return $ Texture tex w h

deleteTexture :: Texture -> IO ()
deleteTexture =
  GL.deleteObjectName . texObject

initTexture :: GL.TextureObject -> IO ()
initTexture tex = do
  GL.textureBinding GL.Texture2D $= Just tex -- glBindTexture
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  GLU.texture2DWrap $= (GL.Mirrored, GL.ClampToEdge)
  GL.textureBinding GL.Texture2D $= Nothing
