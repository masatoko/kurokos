module Kurokos.GUI.Types where

import           Foreign.C.Types (CInt)
import           Linear.V2

data Direction
  = Horizontal
  | Vertical
  deriving Show

type GuiPos = V2 CInt
type GuiSize = V2 CInt
