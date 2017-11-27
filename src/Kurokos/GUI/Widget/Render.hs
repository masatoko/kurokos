{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Widget.Render where

import qualified Control.Exception.Safe as E

import qualified SDL
import qualified SDL.Font               as Font

import           Kurokos.GUI.Def        (RenderEnv (..))
import           Kurokos.GUI.Import
import           Kurokos.GUI.Widget

createTextureFromWidget :: (MonadIO m, RenderEnv m, MonadMask m) => Widget -> m (V2 CInt, SDL.Texture)
createTextureFromWidget Label{..} = do
  (w,h) <- Font.size wFont wTitle
  let size = fromIntegral <$> V2 w h
  texture <- withRenderer $ \r ->
    E.bracket
      (Font.blended wFont (V4 255 255 255 255) wTitle)
      SDL.freeSurface
      (SDL.createTextureFromSurface r)
  return (size, texture)
