{-# LANGUAGE FlexibleContexts #-}

module Kurokos.Texture
  ( allocTexture
  , allocTextureB
  -- ** Texture State
  , setBlendMode
  , setAlphaMod
  , setColorMod
  ) where

import qualified Control.Exception.Safe      as E
import           Control.Exception.Safe      (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Reader
import           Data.ByteString      (ByteString)
import           Data.Text            (Text)
import           Data.Word            (Word8)
import           Linear.V2
import           Linear.V3
import           Linear.V4
import           Control.Monad.Trans.Resource (ResourceT, ReleaseKey, allocate, MonadResource)

import qualified SDL
import qualified SDL.Image            as Image
import           SDL                  (($=), get)

import           Kurokos.Core

-- Allocate

allocTexture :: (MonadReader KurokosEnv m, MonadIO m, MonadMask m, MonadBase IO m) => FilePath -> ResourceT m (ReleaseKey, SDL.Texture)
allocTexture path = do
  env <- lift getEnv
  allocate (load env) SDL.destroyTexture
  where
    load env =
      runKurokosEnvT env $
        withRenderer $ \r -> Image.loadTexture r path

allocTextureB :: (MonadReader KurokosEnv m, MonadIO m, MonadMask m, MonadBase IO m) => ByteString -> ResourceT m (ReleaseKey, SDL.Texture)
allocTextureB byte = do
  env <- lift getEnv
  allocate (load env) SDL.destroyTexture
  where
    load env =
      runKurokosEnvT env $
        withRenderer $ \r -> Image.decodeTexture r byte

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
