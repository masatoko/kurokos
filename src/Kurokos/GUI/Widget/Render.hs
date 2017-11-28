{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Widget.Render where

import qualified Control.Exception  as E

import qualified SDL
import qualified SDL.Font           as Font
import qualified SDL.Primitive      as Prim

import           Kurokos.GUI.Import
import           Kurokos.GUI.Types
import           Kurokos.GUI.Widget

renderWidget :: SDL.Renderer -> GuiSize -> WidgetColor -> Widget -> IO ()
renderWidget r parentSize WidgetColor{..} Label{..} = do
  Prim.roundRectangle r (pure 0) ((+ (-1)) <$> parentSize) 3 (V4 0 0 255 100) -- test
  --
  (w,h) <- Font.size wFont wTitle
  let size = fromIntegral <$> V2 w h
  tex <- E.bracket
    (Font.blended wFont wcFont wTitle)
    SDL.freeSurface
    (SDL.createTextureFromSurface r)
  let pos = P $ (`div` 2) <$> parentSize - size
  SDL.copy r tex Nothing $ Just (Rectangle pos size)

renderWidget r parentSize WidgetColor{..} Button{..} = do
  (w,h) <- Font.size wFont wTitle
  let size = fromIntegral <$> V2 w h
  tex <- E.bracket
    (Font.blended wFont wcFont wTitle)
    SDL.freeSurface
    (SDL.createTextureFromSurface r)
  --
  let size' = (+ (-1)) <$> parentSize
  Prim.fillRoundRectangle r (pure 0) size' 3 wcBack
  Prim.roundRectangle r (pure 0) size' 5 wcTint
  --
  let pos = P $ (`div` 2) <$> parentSize - size
  SDL.copy r tex Nothing $ Just (Rectangle pos size)
