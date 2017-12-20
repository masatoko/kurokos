{-# LANGUAGE RankNTypes #-}
module Kurokos.Graphics.Texture
  ( readTexture
  -- , decodeTexture
  ) where

import qualified Codec.Picture             as Pic
import qualified Codec.Picture.Extra       as Pic
import           Codec.Picture.Types       (convertImage)
import qualified Control.Exception         as E
import qualified Data.ByteString           as BS
import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

readTexture :: FilePath -> IO (Either String GL.TextureObject)
readTexture fpath =
  either (return . Left) work =<< GLU.readTexture fpath
  where
    work tex = do
      initTexture tex
      return $ Right tex

-- readTexture :: FilePath -> IO (Either String GL.TextureObject)
-- readTexture fpath = do
--   bytes <- BS.readFile fpath
--   decodeTexture bytes GLU.loadTexture
--
-- decodeTexture :: BS.ByteString -> (forall a. GLU.IsPixelData a => GLU.TexInfo a -> IO GL.TextureObject) -> IO (Either String GL.TextureObject)
-- decodeTexture bytes k =
--   case Pic.decodeImage bytes of
--     Left err           -> return $ Left err
--     Right dynamicImage -> conv dynamicImage
--   where
--     -- conv: Original: https://github.com/acowley/GLUtil/blob/master/src/Graphics/GLUtil/JuicyTextures.hs
--     conv (Pic.ImageY8 img)     = Right <$> work (flipImg img GLU.TexMono)
--     conv (Pic.ImageYF img)     = Right <$> work (flipImg img GLU.TexMono)
--     conv (Pic.ImageYA8 _)      = return $ Left "YA format not supported"
--     conv (Pic.ImageRGB8 img)   = Right <$> work (flipImg img GLU.TexRGB)
--     conv (Pic.ImageRGBF img)   = Right <$> work (flipImg img GLU.TexRGB)
--     conv (Pic.ImageRGBA8 img)  = Right <$> work (flipImg img GLU.TexRGBA)
--     conv (Pic.ImageYCbCr8 img) = conv . Pic.ImageRGB8 $ convertImage img
--     conv _                     = return $ Left "Unsupported image format"
--
--     flipImg img tp = GLU.texInfo w h tp p
--       where
--         Pic.Image w h p = Pic.flipVertically img
--
--     work tex = do
--       tex' <- k tex
--       initTexture tex'
--       return tex'
--       where

initTexture tex = do
  GL.textureBinding GL.Texture2D $= Just tex -- glBindTexture
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  GLU.texture2DWrap $= (GL.Mirrored, GL.ClampToEdge)
  GL.textureBinding GL.Texture2D $= Nothing
