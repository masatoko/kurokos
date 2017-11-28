{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Widget.Render where

import qualified Control.Exception  as E

import qualified SDL
import qualified SDL.Font           as Font
import qualified SDL.Primitive            as Gfx

import           Kurokos.GUI.Def    (RenderEnv (..))
import           Kurokos.GUI.Import
import           Kurokos.GUI.Types
import           Kurokos.GUI.Widget

renderWidget :: SDL.Renderer -> GuiSize -> Widget -> IO ()
renderWidget r parentSize Label{..} = do
  Gfx.fillRoundRectangle r (pure 0) parentSize 10 (V4 0 0 255 10) -- test
  --
  (w,h) <- Font.size wFont wTitle
  let size = fromIntegral <$> V2 w h
  tex <- E.bracket
    (Font.blended wFont (V4 255 255 255 255) wTitle)
    SDL.freeSurface
    (SDL.createTextureFromSurface r)
  SDL.copy r tex Nothing $ Just (SDL.Rectangle (pure 0) size)
