{-# LANGUAGE GADTs           #-}
module Kurokos.UI.Widget where

import           Control.Lens
import           Data.Text         (Text)
import qualified Data.Text         as T

import qualified SDL

import           Kurokos.UI.Def    (Renderable (..))
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import qualified Kurokos.Graphics as G
import qualified Kurokos.Graphics.Font as Font

data Value
  = ValueI Int Int Int
  | ValueF Float Float Float
  | ValueD Double Double Double
  deriving Show

data Widget where
  Transparent :: Widget
  Fill        :: Widget
  Label       :: Text -> Font.Font -> G.FontSize -> Widget
  ImageView   :: G.Texture -> Widget
  Button      :: Text -> Font.Font -> G.FontSize -> Widget
  Switch      :: Text -> Font.Font -> G.FontSize -> Bool -> Widget
  Slider      :: Text -> Font.Font -> G.FontSize -> Value -> Widget
  UserWidget  :: Renderable a => a -> Widget

instance Show Widget where
  show Transparent  = "<TRS>"
  show Fill         = "<FIL>"
  show Label{}      = "<LBL>"
  show ImageView{}  = "<IMG>"
  show Button{}     = "<BTN>"
  show Switch{}     = "<SWT>"
  show Slider{}     = "<SLD>"
  show UserWidget{} = "<USR>"

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

attribOf Switch{} =
  defAttrib
    & hoverable .~ True
    & clickable .~ True

attribOf Slider{} =
  defAttrib
    & hoverable .~ True
    & clickable .~ True

attribOf UserWidget{} =
  defAttrib
    & hoverable .~ False
    & clickable .~ False
