module Kurokos.GUI.Widget.Make where

import           Data.Text

import qualified SDL

import           Kurokos.GUI.Core
import           Kurokos.GUI.Import
import           Kurokos.GUI.Widget

newTransparent :: Monad m => GuiT m Widget
newTransparent = return Transparent

newFill :: Monad m => GuiT m Widget
newFill = return Fill

newLabel :: Monad m => Text -> GuiT m Widget
newLabel title = do
  font <- asks geFont
  return Label
    { wTitle = title
    , wFont = font
    }

newImageView :: Monad m => SDL.Texture -> GuiT m Widget
newImageView = return . ImageView

newButton :: Monad m => Text -> GuiT m Widget
newButton title = do
  font <- asks geFont
  return Button
    { wTitle = title
    , wFont = font
    }
