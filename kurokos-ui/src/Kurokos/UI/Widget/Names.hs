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
widgetNameOf Switch{}      = wnameSwitch
widgetNameOf Slider{}      = wnameSlider
widgetNameOf TextField{}   = wnameTextField
widgetNameOf Picker{}      = wnamePicker
widgetNameOf UserWidget{}  = wnameUserWidget

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

wnameSwitch :: WidgetName
wnameSwitch = "switch"

wnameSlider :: WidgetName
wnameSlider = "slider"

wnameTextField :: WidgetName
wnameTextField = "text-field"

wnamePicker :: WidgetName
wnamePicker = "picker"

wnameUserWidget :: WidgetName
wnameUserWidget = "user"
