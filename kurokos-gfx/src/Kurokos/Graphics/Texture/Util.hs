{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |Utilities for loading texture data.
module Kurokos.Graphics.Texture.Util where

-- Original: https://github.com/acowley/GLUtil/blob/master/src/Graphics/GLUtil/Textures.hs

import           Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL.GL.VertexArrays as GL

import           Graphics.GLUtil                           (Elem (..),
                                                            IsPixelData,
                                                            TexColor (..),
                                                            TexInfo (..))
import           Graphics.GLUtil.TypeMapping               (HasGLType (..))

-- |Replace a texture's pixel data with data from a 'TexInfo'.
reloadTextureWith :: forall a t1 t2. (IsPixelData a, BindableTextureTarget t1, TwoDimensionalTextureTarget t2) => t1 -> t2 -> TextureObject -> TexInfo a -> IO ()
reloadTextureWith bndTarget tdTarget obj tex = do
  textureBinding bndTarget $= Just obj
  loadTex $ texColor tex
  where loadTex TexMono = case pixelType of
                            GL.UnsignedShort -> loadAux Luminance16 Luminance
                            GL.Float         -> loadAux R32F Red
                            GL.HalfFloat     -> loadAux R16F Red
                            GL.UnsignedByte  -> loadAux R8 Red
                            _                -> loadAux Luminance' Luminance
        loadTex TexRG = case pixelType of
                          GL.UnsignedShort -> loadAux RG16 RGInteger
                          GL.Float -> loadAux RG32F RG
                          GL.HalfFloat -> loadAux RG16F RG
                          GL.UnsignedByte -> loadAux RG8UI RGInteger
                          GL.Byte -> loadAux RG8I RGInteger
                          GL.Int -> loadAux RG32I RGInteger
                          GL.UnsignedInt -> loadAux RG32UI RGInteger
                          _ -> error "Unknown pixelType for TexRG"
        loadTex TexRGB = loadAux RGBA' RGB
        loadTex TexBGR = loadAux RGBA' BGR
        loadTex TexRGBA = loadAux RGBA' RGBA
        sz = TextureSize2D (texWidth tex) (texHeight tex)
        pixelType = glType (undefined :: Elem a)
        loadAux i e =
          withPixels
            (texData tex)
            (texImage2D tdTarget NoProxy 0 i sz 0 . PixelData e pixelType)
