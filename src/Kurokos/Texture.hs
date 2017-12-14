{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Kurokos.Texture
  (
  -- ** Texture State
    setBlendMode
  , setAlphaMod
  , setColorMod
  ) where

import           Control.Monad.Reader
import           Data.Word                    (Word8)
import           Linear.V3

import           SDL                          (($=))
import qualified SDL

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
