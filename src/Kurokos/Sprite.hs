{-# LANGUAGE FlexibleContexts #-}

module Kurokos.Sprite
  ( allocTexture
  , loadSprite
  , decodeSprite
  , freeSprite
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
import qualified SDL.Image
import           SDL                  (($=), get)
import qualified SDL.Font             as Font

import           Kurokos.Core
import           Kurokos.Data        (Font, Sprite (..))

-- TODO: Change color
-- newSprite :: (MonadReader KurokosEnv m, MonadIO m) => Font -> V4 Word8 -> Text -> m Sprite
-- newSprite font color text = do
--   (w,h) <- Font.size font text
--   withRenderer $ \rndr -> do
--     texture <- E.bracket (Font.blended font color text)
--                          SDL.freeSurface
--                          (SDL.createTextureFromSurface rndr)
--     return $ Sprite texture (V2 (fromIntegral w) (fromIntegral h))

allocTexture :: (MonadReader KurokosEnv m, MonadIO m, MonadMask m, MonadBase IO m) => FilePath -> ResourceT m (ReleaseKey, SDL.Texture)
allocTexture path = do
  env <- lift getEnv
  allocate (load env) SDL.destroyTexture
  where
    load env =
      runKurokosEnvT env $
        withRenderer $ \r -> SDL.Image.loadTexture r path

freeSprite :: MonadIO m => Sprite -> m ()
freeSprite (Sprite t _) = SDL.destroyTexture t

loadSprite :: (MonadReader KurokosEnv m, MonadIO m, MonadMask m) => FilePath -> V2 Int -> m Sprite
loadSprite path size =
  withRenderer $ \r -> do
    texture <- SDL.Image.loadTexture r path
    return $ Sprite texture $ fromIntegral <$> size

decodeSprite :: (MonadReader KurokosEnv m, MonadIO m, MonadMask m) => ByteString -> V2 Int -> m Sprite
decodeSprite bytes size =
  withRenderer $ \r -> do
    texture <- SDL.Image.decodeTexture r bytes
    return $ Sprite texture $ fromIntegral <$> size

setBlendMode :: MonadIO m => Sprite -> SDL.BlendMode -> m ()
setBlendMode s mode =
  SDL.textureBlendMode (sptex s) $= mode

setAlphaMod :: MonadIO m => Sprite -> Word8 -> m ()
setAlphaMod s alpha =
  SDL.textureAlphaMod (sptex s) $= alpha

setColorMod :: MonadIO m => Sprite -> V3 Word8 -> m ()
setColorMod s rgb =
  SDL.textureColorMod (sptex s) $= rgb
