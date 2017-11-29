module Kurokos.GUI.Event where

import Kurokos.GUI.Import
import Kurokos.GUI.Widget

import qualified SDL

data GuiEvent = GuiEvent
  { geType :: EventType
  , geWidget :: Widget
  } deriving Show

data EventType
  = SelectEvent
      { seInputMotion :: SDL.InputMotion
      }
  deriving Show
