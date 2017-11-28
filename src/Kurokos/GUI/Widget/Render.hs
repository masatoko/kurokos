{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Widget.Render where

import qualified Control.Exception  as E

import qualified SDL
import qualified SDL.Font           as Font
import qualified SDL.Primitive            as Gfx

import           Kurokos.GUI.Import
import           Kurokos.GUI.Types
import           Kurokos.GUI.Widget

renderWidget :: SDL.Renderer -> GuiSize -> Widget -> IO ()
renderWidget r parentSize Label{..} = do
  Gfx.roundRectangle r (pure 0) ((+ (-1)) <$> parentSize) 10 (V4 0 0 255 100) -- test
  --
  (w,h) <- Font.size wFont wTitle
  let size = fromIntegral <$> V2 w h
  tex <- E.bracket
    (Font.blended wFont (V4 255 255 255 255) wTitle)
    SDL.freeSurface
    (SDL.createTextureFromSurface r)
  let pos = P $ (`div` 2) <$> parentSize - size
  SDL.copy r tex Nothing $ Just (SDL.Rectangle pos size)

renderWidget r parentSize Button{..} = do
  (w,h) <- Font.size wFont wTitle
  let size = fromIntegral <$> V2 w h
  tex <- E.bracket
    (Font.blended wFont (V4 255 255 255 255) wTitle)
    SDL.freeSurface
    (SDL.createTextureFromSurface r)
  --
  let size' = (+ (-1)) <$> parentSize
  Gfx.fillRoundRectangle r (pure 0) size' 5 (V4 50 50 50 255)
  Gfx.roundRectangle r (pure 0) size' 5 (V4 100 100 100 255)
  --
  let pos = P $ (`div` 2) <$> parentSize - size
  SDL.copy r tex Nothing $ Just (SDL.Rectangle pos size)
