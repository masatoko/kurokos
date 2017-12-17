{-# LANGUAGE GADTs           #-}
module Kurokos.UI.Widget where

import           Control.Lens
import           Data.Text         (Text)
import qualified Data.Text         as T

import qualified SDL
import           SDL.Font          (Font)

import           Kurokos.UI.Def    (Renderable (..))
import           Kurokos.UI.Import
import           Kurokos.UI.Types

data Widget where
  Transparent :: Widget
  Fill        :: Widget
  Label       :: Text -> Font -> Widget
  ImageView   :: SDL.Texture -> Widget
  Button      :: Text -> Font -> Widget
  UserWidget  :: Renderable a => a -> Widget

instance Show Widget where
  show Transparent      = "<TRS>"
  show Fill             = "<FIL>"
  show (Label title _)  = "<LBL:" ++ T.unpack title ++ ">"
  show ImageView{}      = "<IMG>"
  show (Button title _) = "<BTN:" ++ T.unpack title ++ ">"
  show UserWidget{}     = "<USR>"

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

attribOf UserWidget{} =
  defAttrib
    & hoverable .~ False
    & clickable .~ False
