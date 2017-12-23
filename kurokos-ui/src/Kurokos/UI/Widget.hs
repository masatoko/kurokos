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

data Widget where
  Transparent :: Widget
  Fill        :: Widget
  Label       :: G.TextTexture -> Widget
  ImageView   :: G.Texture -> Widget
  Button      :: G.TextTexture -> Widget
  UserWidget  :: Renderable a => a -> Widget

instance Show Widget where
  show Transparent  = "<TRS>"
  show Fill         = "<FIL>"
  show Label{}      = "<LBL>"
  show ImageView{}  = "<IMG>"
  show Button{}     = "<BTN>"
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

attribOf UserWidget{} =
  defAttrib
    & hoverable .~ False
    & clickable .~ False

-- Free

freeWidget :: MonadIO m => Widget -> m ()
freeWidget (Label texttex)  = liftIO $ G.deleteTextTexture texttex
freeWidget (Button texttex) = liftIO $ G.deleteTextTexture texttex
freeWidget _                = return ()
