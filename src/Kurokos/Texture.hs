{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Kurokos.Texture
  ( allocTexture
  , allocTextureB
  -- ** Texture State
  , setBlendMode
  , setAlphaMod
  , setColorMod
  ) where

import           Control.Monad.Base           (MonadBase)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource (MonadThrow, ReleaseKey,
                                               ResourceT, allocate)
import           Data.ByteString              (ByteString)
import           Data.Word                    (Word8)
import           Linear.V3

import           SDL                          (($=))
import qualified SDL
import qualified SDL.Image                    as Image

import           Kurokos.Core

-- Allocate

allocTexture :: (MonadReader KurokosEnv m, MonadIO m, MonadThrow m, MonadBase IO m)
  => FilePath -> ResourceT m (ReleaseKey, SDL.Texture)
allocTexture path = do
  r <- getRenderer
  allocate (Image.loadTexture r path) SDL.destroyTexture

allocTextureB :: (MonadReader KurokosEnv m, MonadIO m, MonadThrow m, MonadBase IO m)
  => ByteString -> ResourceT m (ReleaseKey, SDL.Texture)
allocTextureB bytes = do
  r <- getRenderer
  allocate (Image.decodeTexture r bytes) SDL.destroyTexture

-- State

setBlendMode :: MonadIO m => SDL.Texture -> SDL.BlendMode -> m ()
setBlendMode t mode =
  SDL.textureBlendMode t $= mode

setAlphaMod :: MonadIO m => SDL.Texture -> Word8 -> m ()
setAlphaMod t alpha =
  SDL.textureAlphaMod t $= alpha

setColorMod :: MonadIO m => SDL.Texture -> V3 Word8 -> m ()
setColorMod t rgb =
  SDL.textureColorMod t $= rgb
