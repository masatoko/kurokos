module Kurokos.UI.Util where

-- import qualified SDL
-- import SDL (($=))

import Kurokos.UI.Import
import Kurokos.UI.Color
import qualified Kurokos.Graphics as G

clearBy :: MonadIO m => G.Renderer -> Color -> m ()
clearBy r color = do
  return ()
  -- SDL.rendererDrawColor r $= color
  -- SDL.clear r
