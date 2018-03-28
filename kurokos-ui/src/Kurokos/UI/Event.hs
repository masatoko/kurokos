module Kurokos.UI.Event where

import           Kurokos.UI.Control (GuiAction)
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget

import qualified SDL

data WidgetInfo = WidgetInfo
  { wifWidget :: Widget
  , wifIdent  :: WTIdent
  , wifName   :: Maybe WTName
  } deriving Show

data GuiEvent
  = Clicked
    { geWidgetInfo :: WidgetInfo
    , gePosition   :: GuiPos
    , geAction     :: GuiAction
    }
  deriving Show
