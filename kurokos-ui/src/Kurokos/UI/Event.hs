module Kurokos.UI.Event where

import Kurokos.UI.Import
import Kurokos.UI.Widget
import Kurokos.UI.Types

import qualified SDL

data GuiEvent = GuiEvent
  { geType :: EventType
  --
  , geWidget :: Widget
  , geWidgetKey :: WTKey
  , geWidgetName :: Maybe String
  } deriving Show

data EventType
  = MouseClick SDL.MouseButtonEventData
  deriving (Eq, Show)
