{-# LANGUAGE TemplateHaskell #-}
module Kurokos.UI.Color where

import           Control.Lens
import           Data.Word    (Word8)

import           SDL.Vect

type Color = V4 Word8

data WidgetColor = WidgetColor
  { _wcBack   :: Color
  , _wcBorder :: Color
  , _wcTitle  :: Color
  , _wcTint   :: Color
  }

makeLenses ''WidgetColor

defaultWidgetColor :: WidgetColor
defaultWidgetColor = WidgetColor
  { _wcBack   = V4 255 255 255 255
  , _wcBorder = V4 220 220 220 255
  , _wcTitle  = V4 54 20 171 255
  , _wcTint   = V4 220 220 220 255
  }

data ContextColor = ContextColor
  { ctxcolNormal :: WidgetColor
  , ctxcolHover  :: WidgetColor
  , ctxcolClick  :: WidgetColor
  }

defaultContextColor :: ContextColor
defaultContextColor = ContextColor
  { ctxcolNormal = defaultWidgetColor
  , ctxcolHover  = defaultWidgetColor & wcBack . _xyz %~ (+ (-10))
  , ctxcolClick  = defaultWidgetColor
  }
