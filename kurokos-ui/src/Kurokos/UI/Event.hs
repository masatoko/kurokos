module Kurokos.UI.Event where

import           Kurokos.UI.Control (GuiAction)
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget

import qualified SDL

data GuiEvent
  = Clicked
    { geWidget   :: Widget
    , geWTIdent  :: WTIdent
    , geWTName   :: Maybe WTName
    --
    , gePosition :: GuiPos
    , geAction   :: GuiAction
    } deriving Show
