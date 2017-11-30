module Kurokos.GUI.Widget.Make where

import           Data.Text

import           Kurokos.GUI.Core
import           Kurokos.GUI.Import
import           Kurokos.GUI.Widget

newFill :: Monad m => GuiT m Widget
newFill = return Fill

newLabel :: Monad m => Text -> GuiT m Widget
newLabel title = do
  font <- asks geFont
  return Label
    { wTitle = title
    , wFont = font
    }

newButton :: Monad m => Text -> GuiT m Widget
newButton title = do
  font <- asks geFont
  return Button
    { wTitle = title
    , wFont = font
    }
