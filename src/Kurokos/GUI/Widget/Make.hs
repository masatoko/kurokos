module Kurokos.GUI.Widget.Make where

import           Data.Text

import           Kurokos.GUI.Core
import           Kurokos.GUI.Import
import           Kurokos.GUI.Widget

newLabel :: Monad m => Text -> GuiT m Widget
newLabel title = do
  font <- asks geFont
  return Label
    { wTitle = title
    , wFont = font
    }
