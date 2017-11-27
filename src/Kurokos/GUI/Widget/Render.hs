{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Widget.Render where

import qualified Control.Exception.Safe as E

import qualified SDL
import qualified SDL.Font               as Font

import           Kurokos.GUI.Def        (RenderEnv (..))
import           Kurokos.GUI.Import
import           Kurokos.GUI.Widget

renderWidget :: (MonadIO m, RenderEnv m, MonadMask m) => Widget -> m ()
renderWidget Label{..} = do
  tex <- withRenderer $ \r ->
    E.bracket
      (Font.blended wFont (V4 255 255 255 255) wTitle)
      SDL.freeSurface
      (SDL.createTextureFromSurface r) -- TODO: Must create once!
  renderTexture tex $ Rectangle (P $ V2 0 0) (V2 50 50)
