module Kurokos.Types
  ( Font
  ) where

import           Foreign.C.Types (CInt)
import           Linear.V2

import qualified SDL
import qualified SDL.Font

type Font = SDL.Font.Font
