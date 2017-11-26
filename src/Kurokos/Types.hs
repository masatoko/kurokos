module Kurokos.Types
  ( Font
  , PadId
  ) where

import           Data.Word       (Word8)
import           Foreign.C.Types (CInt)
import           Linear.V2

import qualified SDL
import qualified SDL.Font

type Font = SDL.Font.Font

type PadId = Word8
