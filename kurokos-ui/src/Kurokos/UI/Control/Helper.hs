module Kurokos.UI.Control.Helper where

import           Control.Lens
import           Data.List.Extra           (firstJust)
import           Data.Maybe                (catMaybes, mapMaybe, maybeToList)
import           Linear.V2
import           Safe                      (lastMay)

import           Kurokos.UI.Control.Cursor
import           Kurokos.UI.Core
import qualified Kurokos.UI.Event          as E
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget
import qualified Kurokos.UI.WidgetTree     as WT

clickedOn :: Eq act => act -> WidgetIdent -> [(act, E.GuiEvent)] -> Maybe GuiPos
clickedOn act wid = firstJust isTarget
  where
    isTarget (a,e)
      | a == act && E.geWidgetId e == Just wid =
        case E.geType e of
          E.Clicked pos -> Just pos
      | otherwise = Nothing
