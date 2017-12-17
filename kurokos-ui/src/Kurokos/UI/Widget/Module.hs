module Kurokos.UI.Widget.Module where

import           Data.Text          (Text)

import           Kurokos.UI.Import
import           Kurokos.UI.Widget

setTitle :: Text -> Widget -> Widget
setTitle _     w@Transparent   = w
setTitle _     w@Fill          = w
setTitle title (Label _ font)  = Label title font
setTitle _     w@ImageView{}   = w
setTitle title (Button _ font) = Button title font
setTitle _     w@UserWidget{}  = w
