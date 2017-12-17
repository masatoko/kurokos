module Kurokos.UI.Event where

import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget

import qualified SDL

data GuiEvent = GuiEvent
  { geType    :: EventType
  --
  , geWidget  :: Widget
  , geWTIdent :: WTIdent
  , geWTName  :: Maybe WTName
  } deriving Show

data EventType
  = Clicked GuiPos
  deriving (Eq, Show)
