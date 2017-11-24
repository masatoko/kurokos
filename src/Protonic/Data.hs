module Protonic.Data
  ( Font (..)
  , Sprite (..)
  ) where

import           Foreign.C.Types (CInt)
import           Linear.V2

import qualified SDL
import           SDL.TTF.FFI     (TTFFont)

data Font = Font TTFFont

data Sprite = Sprite
  { sptex  :: SDL.Texture
  , spsize :: V2 CInt
  }
