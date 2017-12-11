{-# LANGUAGE RecordWildCards #-}
module Kurokos.UI.Widget.Render where

import qualified Control.Exception  as E

import           SDL                (($=))
import qualified SDL
import qualified SDL.Font           as Font
import qualified SDL.Primitive      as Prim

import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget

renderWidget :: SDL.Renderer -> GuiSize -> WidgetColor -> Widget -> IO ()
renderWidget _r _parentSize (WC WP{..}) Transparent = return ()

renderWidget r _parentSize (WC WP{..}) Fill = do
  SDL.rendererDrawColor r $= V4 0 0 0 50
  SDL.clear r

renderWidget r parentSize (WC WP{..}) Label{..} = do
  -- Prim.roundRectangle r (pure 0) ((+ (-1)) <$> parentSize) 3 (V4 0 0 255 100) -- test
  --
  (w,h) <- Font.size wFont wTitle
  let size = fromIntegral <$> V2 w h
  tex <- E.bracket
    (Font.blended wFont wpFont wTitle)
    SDL.freeSurface
    (SDL.createTextureFromSurface r)
  let pos = P $ (`div` 2) <$> parentSize - size
  SDL.copy r tex Nothing $ Just (Rectangle pos size)

renderWidget r _parentSize (WC WP{..}) ImageView{..} =
  SDL.copy r wImage Nothing Nothing

renderWidget r parentSize (WC WP{..}) Button{..} = do
  (w,h) <- Font.size wFont wTitle
  let size = fromIntegral <$> V2 w h
  tex <- E.bracket
    (Font.blended wFont wpFont wTitle)
    SDL.freeSurface
    (SDL.createTextureFromSurface r)
  --
  let size' = (+ (-1)) <$> parentSize
  Prim.fillRoundRectangle r (pure 0) size' 3 wpBack
  Prim.roundRectangle r (pure 0) size' 5 wpTint
  --
  let pos = P $ (`div` 2) <$> parentSize - size
  SDL.copy r tex Nothing $ Just (Rectangle pos size)
