module Kurokos.UI.Widget.Module where

import           Data.Text          (Text)

import           Kurokos.UI.Import
import           Kurokos.UI.Widget

setTitle :: Text -> Widget -> Widget
setTitle _     w@Transparent = w
setTitle _     w@Fill        = w
setTitle title w@Label{}     = w {wTitle = title}
setTitle _     w@ImageView{} = w
setTitle title w@Button{}    = w {wTitle = title}
