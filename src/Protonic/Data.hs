module Protonic.Data
  ( Font
  , Sprite (..)
  ) where

import           Foreign.C.Types (CInt)
import           Linear.V2

import qualified SDL
import qualified SDL.Font

type Font = SDL.Font.Font

data Sprite = Sprite
  { sptex  :: SDL.Texture
  , spsize :: V2 CInt
  }
