module Kurokos.UI.Control.Helper where

import           Control.Lens
import           Data.List.Extra           (firstJust)
import           Data.Maybe                (catMaybes, mapMaybe, maybeToList)
import           Linear.V2
import           Safe                      (lastMay)

import           Kurokos.UI.Control
import           Kurokos.UI.Control.Cursor
import           Kurokos.UI.Core
import qualified Kurokos.UI.Event          as E
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget
import qualified Kurokos.UI.WidgetTree     as WT

clickedOn :: GuiAction -> WTName -> GUI -> Maybe GuiPos
clickedOn act name g = firstJust convTarget (g^.unGui._2.gstEvents)
  where
    convTarget e@E.Clicked{}
      | match     = Just $ E.gePosition e
      | otherwise = Nothing
      where
        mName = E.wifName $ E.geWidgetInfo e
        match = E.geAction e == act && mName == Just name
    convTarget _ = Nothing
