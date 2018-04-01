module Kurokos.UI.Event where

import qualified Data.Text          as T

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
  | Dragging
    { geWidgetInfo :: WidgetInfo
    , geButton     :: SDL.MouseButton
    , geCount      :: Integer
    }
  | DragAndDrop
    { geDrag   :: WidgetInfo
    , geDrop   :: Maybe WidgetInfo
    , geButton :: SDL.MouseButton
    }
  | TextFixed
    { geWidgetInfo :: WidgetInfo
    , geText       :: T.Text
    }
  deriving Show
