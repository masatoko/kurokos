{-# LANGUAGE OverloadedStrings #-}
module Kurokos.UI.Widget.Names
  ( WidgetName
  , widgetNameList
  , widgetNameOf
  ) where

import qualified Data.Text         as T

import           Kurokos.UI.Widget (Widget (..))

type WidgetName = T.Text

widgetNameList :: [WidgetName]
widgetNameList =
  [ wnameTransparent
  , wnameFill
  , wnameLabel
  , wnameImageView
  , wnameButton
  ]

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
