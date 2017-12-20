{-# LANGUAGE LambdaCase #-}
module Kurokos.Graphics.Texture
  ( Texture (..)
  , readTexture
  , decodeTexture
  ) where

import qualified Codec.Picture             as Pic
import           Codec.Picture.Types       (convertImage)
import qualified Control.Exception         as E
import qualified Data.ByteString           as BS
import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

data Texture = Texture
  { texObject :: GL.TextureObject
  , texWidth :: Int
  , texHeight :: Int
  }

readTexture :: FilePath -> IO (Either String Texture)
readTexture fpath =
  decodeTexture =<< BS.readFile fpath

decodeTexture :: BS.ByteString -> IO (Either String Texture)
decodeTexture bytes =
  case Pic.decodeImage bytes of
    Left err -> return $ Left err
    Right dynamicImage -> do
      let image = Pic.convertRGBA8 dynamicImage
          w = Pic.imageWidth image
          h = Pic.imageHeight image
          p = Pic.imageData image
      tex <- GLU.loadTexture $ GLU.texInfo w h GLU.TexRGBA p
      initTexture tex
      return $ Right $ Texture tex w h

initTexture tex = do
  GL.textureBinding GL.Texture2D $= Just tex -- glBindTexture
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  GLU.texture2DWrap $= (GL.Mirrored, GL.ClampToEdge)
  GL.textureBinding GL.Texture2D $= Nothing
