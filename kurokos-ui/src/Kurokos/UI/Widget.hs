{-# LANGUAGE GADTs #-}
module Kurokos.UI.Widget where

import           Control.Lens
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Zipper      as TZ
import           Text.Printf           (printf)

import qualified SDL

import qualified Kurokos.Graphics      as G
import qualified Kurokos.Graphics.Font as Font
import           Kurokos.UI.Def        (Renderable (..))
import           Kurokos.UI.Import
import           Kurokos.UI.Types

data Value
  = ValueI Int Int Int
  | ValueF Float Float Float
  | ValueD Double Double Double
  deriving Show

showValue :: Value -> String
showValue (ValueI v _ _) = show v
showValue (ValueF v _ _) = printf "%.2f" v
showValue (ValueD v _ _) = printf "%.2f" v

valueInt :: Value -> Int
valueInt (ValueI v _ _) = v
valueInt (ValueF v _ _) = round v
valueInt (ValueD v _ _) = round v

valueFloat :: Value -> Float
valueFloat (ValueI v _ _) = fromIntegral v
valueFloat (ValueF v _ _) = v
valueFloat (ValueD v _ _) = realToFrac v

valueDouble :: Value -> Double
valueDouble (ValueI v _ _) = fromIntegral v
valueDouble (ValueF v _ _) = realToFrac v
valueDouble (ValueD v _ _) = v

rateFromValue :: Value -> Double
rateFromValue (ValueI v vmin vmax) = fromIntegral (v - vmin) / fromIntegral (vmax - vmin)
rateFromValue (ValueF v vmin vmax) = realToFrac $ (v - vmin) / (vmax - vmin)
rateFromValue (ValueD v vmin vmax) = (v - vmin) / (vmax - vmin)

updateValueByRate :: Double -> Value -> Value
updateValueByRate rate (ValueI _ vmin vmax) = ValueI v vmin vmax
  where v = vmin + round (rate * fromIntegral (vmax - vmin))
updateValueByRate rate (ValueF _ vmin vmax) = ValueF v vmin vmax
  where v = vmin + realToFrac rate * (vmax - vmin)
updateValueByRate rate (ValueD _ vmin vmax) = ValueD v vmin vmax
  where v = vmin + rate * (vmax - vmin)

data SliderResource = SliderResource { sliderRscKnob :: G.Prim, sliderRscText :: G.Texture } deriving Show

data TextFieldResource = TextFieldResource { txtFldRscCursor :: G.Prim, txtFldRscLeft :: Maybe G.Texture, txtFldRscRight :: Maybe G.Texture } deriving Show

data Widget where
  Transparent :: Widget
  Fill        :: Widget
  Label       :: Text -> Font.Font -> G.FontSize -> Widget
  ImageView   :: G.Texture -> Widget
  Button      :: Text -> Font.Font -> G.FontSize -> Widget
  Switch      :: Text -> Font.Font -> G.FontSize -> Bool -> Widget
  Slider      :: Text -> Font.Font -> G.FontSize -> Maybe SliderResource -> Value -> Widget
  TextField   :: Font.Font -> G.FontSize -> TZ.TextZipper T.Text -> Maybe TextFieldResource -> Widget
  Picker      :: [(String, Text)] -> Font.Font -> G.FontSize -> Int -> [G.Texture] -> Widget
  UserWidget  :: Renderable a => a -> Widget

instance Show Widget where
  show Transparent  = "<TRS>"
  show Fill         = "<FIL>"
  show Label{}      = "<LBL>"
  show ImageView{}  = "<IMG>"
  show Button{}     = "<BTN>"
  show Switch{}     = "<SWT>"
  show Slider{}     = "<SLD>"
  show TextField{}  = "<TXF>"
  show Picker{}     = "<PKR>"
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

attribOf TextField{} =
  defAttrib
    & hoverable .~ True
    & clickable .~ True

attribOf Picker{} =
  defAttrib
    & hoverable .~ True
    & clickable .~ True

attribOf UserWidget{} =
  defAttrib
    & hoverable .~ False
    & clickable .~ False

additionalClickableSize :: WContext -> Widget -> Maybe (V2 CInt)
additionalClickableSize ctx = work
  where
    V2 w h = wstSize $ ctx^.ctxWidgetState
    focus = ctx^.ctxWidgetState.wstFocus

    work (Picker ts font size _ _)
      | focus     = Just $ V2 w (h * n)
      | otherwise = Just $ V2 w h
      where
        n = fromIntegral $ length ts
    work _ = Nothing
