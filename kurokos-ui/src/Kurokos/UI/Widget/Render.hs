{-# LANGUAGE RecordWildCards #-}
module Kurokos.UI.Widget.Render where

import qualified Control.Exception   as E
import           Control.Monad.Extra (whenJust)

import           SDL                 (($=))
import qualified SDL
import qualified SDL.Font            as Font
import qualified SDL.Primitive       as Prim

import           Kurokos.UI.Color
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Util
import           Kurokos.UI.Widget

renderWidget :: SDL.Renderer -> GuiSize -> WidgetColor -> Widget -> IO ()
renderWidget _r _parentSize _ Transparent = return ()

renderWidget r _parentSize WidgetColor{..} Fill =
  clearBy r _wcBack

renderWidget r parentSize WidgetColor{..} Label{..} = do
  (w,h) <- Font.size wFont wTitle
  let size = fromIntegral <$> V2 w h
  tex <- E.bracket
    (Font.blended wFont _wcTitle wTitle)
    SDL.freeSurface
    (SDL.createTextureFromSurface r)
  let pos = P $ (`div` 2) <$> parentSize - size
  SDL.copy r tex Nothing $ Just (Rectangle pos size)

renderWidget r _parentSize WidgetColor{..} ImageView{..} = do
  clearBy r _wcBack
  SDL.copy r wImage Nothing Nothing

renderWidget r parentSize WidgetColor{..} Button{..} = do
  (w,h) <- Font.size wFont wTitle
  let size = fromIntegral <$> V2 w h
  tex <- E.bracket
    (Font.blended wFont _wcTitle wTitle)
    SDL.freeSurface
    (SDL.createTextureFromSurface r)
  --
  let size' = (+ (-1)) <$> parentSize
  Prim.fillRoundRectangle r (pure 0) size' 3 _wcBack
  Prim.roundRectangle r (pure 0) size' 5 _wcBorder
  --
  let pos = P $ (`div` 2) <$> parentSize - size
  SDL.copy r tex Nothing $ Just (Rectangle pos size)
