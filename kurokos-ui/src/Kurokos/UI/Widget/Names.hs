module Kurokos.UI.Widget.Names where

import qualified Data.Text         as T

import           Kurokos.UI.Widget (Widget (..))

type WidgetName = String

widgetNameOf :: Widget -> WidgetName
widgetNameOf Transparent{} = wnameTransparent
widgetNameOf Fill{}        = wnameFill
widgetNameOf Label{}       = wnameLabel
widgetNameOf ImageView{}   = wnameImageView
widgetNameOf Button{}      = wnameButton

-- Inernal

wnameTransparent :: WidgetName
wnameTransparent = "trans"

wnameFill :: WidgetName
wnameFill = "fill"

wnameLabel :: WidgetName
wnameLabel = "label"

wnameImageView :: WidgetName
wnameImageView = "image"

wnameButton :: WidgetName
wnameButton = "button"
