module Kurokos.UI.Def where

import           Foreign.C.Types            (CInt)
import           Linear.V2

import qualified SDL

class RenderEnv m where
  getWindow :: m SDL.Window
  getWindowSize :: m (V2 CInt)
  getRenderer :: m SDL.Renderer
  withRenderer :: (SDL.Renderer -> IO a) -> m a
  renderTexture :: SDL.Texture -> SDL.Rectangle CInt -> m ()

class HasEvent m where
  getEvents :: m [SDL.EventPayload]