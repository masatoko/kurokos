{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Widget.Render where

import qualified Control.Exception.Safe as E

import qualified SDL
import qualified SDL.Font               as Font

import           Kurokos.GUI.Def        (RenderEnv (..))
import           Kurokos.GUI.Import
import           Kurokos.GUI.Widget

createTextureFromWidget :: (MonadIO m, RenderEnv m, MonadMask m) => Widget -> m SDL.Texture
createTextureFromWidget Label{..} =
  withRenderer $ \r ->
    E.bracket
      (Font.blended wFont (V4 255 255 255 255) wTitle)
      SDL.freeSurface
      (SDL.createTextureFromSurface r)
