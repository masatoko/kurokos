{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Widget where

import           Control.Lens
import           Data.Text         (Text)
import qualified Data.Text         as T

import qualified SDL
import           SDL.Font          (Font)

import           Kurokos.GUI.Types

data Widget
  = Transparent
  | Fill
  | Label
    { wTitle :: Text
    , wFont  :: Font
    }
  | ImageView
    { wImage :: SDL.Texture
    }
  | Button
    { wTitle :: Text
    , wFont  :: Font
    }

instance Show Widget where
  show Transparent   = "<T>"
  show Fill          = "<F>"
  show Label{..}     = "<L:" ++ T.unpack wTitle ++ ">"
  show ImageView{..} = "<IMG>"
  show Button{..}    = "<B:" ++ T.unpack wTitle ++ ">"

attribOf :: Widget -> WidgetAttrib
attribOf Transparent =
  defAttrib
    & hoverable .~ False
    & clickable .~ True

attribOf Fill{} =
  defAttrib
    & hoverable .~ False
    & clickable .~ True

attribOf Label{} =
  defAttrib
    & hoverable .~ False
    & clickable .~ False

attribOf ImageView{} =
  defAttrib
    & hoverable .~ False
    & clickable .~ False

attribOf Button{} =
  defAttrib
    & hoverable .~ True
    & clickable .~ True
