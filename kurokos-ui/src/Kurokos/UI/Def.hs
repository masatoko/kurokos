module Kurokos.UI.Def where

import           Foreign.C.Types  (CInt)
import           Linear.V2

import qualified SDL

import qualified Kurokos.Graphics as G
import           Kurokos.UI.Types (GuiSize, Style)

class RenderEnv m where
  getWindow :: m SDL.Window
  getWindowSize :: m (V2 CInt)
  getRenderer :: m G.Renderer
  withRenderer :: (G.Renderer -> IO a) -> m a
  -- renderTexture :: G.Texture -> V2 Int -> V2 Int -> m ()

class Renderable a where
  renderW :: G.Renderer -> V2 Int -> V2 Int -> Style -> a -> IO ()
  -- ^ Rerender user data
  needsRender :: a -> IO Bool
  -- ^ Returns whether this content shold be rerendered or not. It's called in `readyRender`.
