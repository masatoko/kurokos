{-# LANGUAGE LambdaCase #-}
module Kurokos.Graphics.Texture
  ( Texture (..)
  , readTexture
  , decodeTexture
  , decodeTexInfo
  , deleteTexture
  --
  , newTexCoordVbo
  ) where

import qualified Codec.Picture             as Pic
import qualified Control.Exception         as E
import qualified Data.ByteString           as BS
import qualified Data.Vector.Storable      as V
import           Data.Word                 (Word8)
import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Kurokos.Graphics.Types
import           Kurokos.Graphics.Vect

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

decodeTexInfo :: BS.ByteString -> IO (GLU.TexInfo (V.Vector Word8))
decodeTexInfo bytes =
  case Pic.decodeImage bytes of
    Left err -> E.throwIO $ userError err
    Right dynamicImage -> do
      let image = Pic.convertRGBA8 dynamicImage
          w = Pic.imageWidth image
          h = Pic.imageHeight image
          p = Pic.imageData image
      return $ GLU.texInfo w h GLU.TexRGBA p

deleteTexture :: Texture -> IO ()
deleteTexture =
  GL.deleteObjectName . texObject

initTexture :: GL.TextureObject -> IO ()
initTexture tex = do
  GL.textureBinding GL.Texture2D $= Just tex -- glBindTexture
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Nearest)
  GLU.texture2DWrap $= (GL.Repeated, GL.Repeat)
  GL.textureBinding GL.Texture2D $= Nothing

newTexCoordVbo :: Texture -> Point V2 Int -> V2 Int -> IO (TypedBufferObject TagVec2)
newTexCoordVbo (Texture _ texw texh) pos size =
  TBO <$> GLU.makeBuffer GL.ArrayBuffer ps
  where
    texw' = fromIntegral texw
    texh' = fromIntegral texh
    P (V2 x y) = fromIntegral <$> pos
    V2 w h = fromIntegral <$> size
    x1 = x / texw'
    y1 = y / texh'
    x2 = (x + w) / texw'
    y2 = (y + h) / texh'
    ps :: [GL.GLfloat]
    ps = [x1, y1, x2, y1, x1, y2, x2, y2]
