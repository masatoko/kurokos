module Kurokos.UI.Def where

import           Foreign.C.Types  (CInt)
import           Linear.V2

import qualified SDL

import           Kurokos.UI.Color (WidgetColor)
import           Kurokos.UI.Types (GuiSize)

class RenderEnv m where
  getWindow :: m SDL.Window
  getWindowSize :: m (V2 CInt)
  getRenderer :: m SDL.Renderer
  withRenderer :: (SDL.Renderer -> IO a) -> m a
  renderTexture :: SDL.Texture -> SDL.Rectangle CInt -> m ()

class Renderable a where
  renderW :: SDL.Renderer -> GuiSize -> WidgetColor -> a -> IO ()
  -- ^ Rerender user data
  needsRender :: a -> IO Bool
  -- ^ Returns whether this content shold be rerendered or not. It's called in `readyRender`.
