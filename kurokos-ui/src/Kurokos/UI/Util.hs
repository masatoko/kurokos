module Kurokos.UI.Util where

import qualified SDL
import SDL (($=))

import Kurokos.UI.Import
import Kurokos.UI.Color

clearBy :: MonadIO m => SDL.Renderer -> Color -> m ()
clearBy r color = do
  SDL.rendererDrawColor r $= color
  SDL.clear r
