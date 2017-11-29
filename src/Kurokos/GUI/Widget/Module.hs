module Kurokos.GUI.Widget.Module where

import           Data.Text          (Text)

import           Kurokos.GUI.Import
import           Kurokos.GUI.Widget

setTitle :: Text -> Widget -> Widget
setTitle title w@Label{}  = w {wTitle = title}
setTitle title w@Button{} = w {wTitle = title}
