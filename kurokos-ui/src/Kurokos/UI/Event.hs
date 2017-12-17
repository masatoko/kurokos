module Kurokos.UI.Event where

import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget

import qualified SDL

data GuiEvent = GuiEvent
  { geType        :: EventType
  --
  , geWidget      :: Widget
  , geWidgetIdent :: WTIdent
  , geWidgetName  :: Maybe WidgetName
  } deriving Show

data EventType
  = Clicked GuiPos
  deriving (Eq, Show)
