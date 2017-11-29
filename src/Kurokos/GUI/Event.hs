module Kurokos.GUI.Event where

import Kurokos.GUI.Import
import Kurokos.GUI.Widget
import Kurokos.GUI.Types

import qualified SDL

data GuiEvent = GuiEvent
  { geType :: EventType
  --
  , geWidget :: Widget
  , geWidgetKey :: WTKey
  , geWidgetName :: Maybe String
  } deriving Show

data EventType
  = SelectEvent
      { seInputMotion :: SDL.InputMotion
      }
  deriving Show
