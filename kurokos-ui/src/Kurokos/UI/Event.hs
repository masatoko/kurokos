module Kurokos.UI.Event where

import qualified Data.Text             as T

import           Kurokos.UI.Control    (GuiAction)
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget
import           Kurokos.UI.WidgetTree (WidgetTreePath)

import qualified SDL

data WidgetInfo = WidgetInfo
  { wifWidget :: Widget
  , wifIdent  :: WTIdent
  , wifName   :: Maybe WTName
  , wifPath   :: WidgetTreePath
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
  | ScrollingByDrag
    { geWidgetInfo :: WidgetInfo
    , geButton     :: SDL.MouseButton
    , gePosition   :: GuiPos
    , geMove       :: V2 CInt
    , geCount      :: Integer
    }
  | Focused
    { geWidgetInfo :: WidgetInfo
    }
  | Unfocused
    { geWidgetInfo :: WidgetInfo
    }
  | TextFixed
    { geWidgetInfo :: WidgetInfo
    , geText       :: T.Text
    }
  | PickerPicked
    { geWidgetInfo :: WidgetInfo
    , geKey        :: String
    , geText       :: T.Text
    }
  deriving Show
