module Kurokos.GUI.Def where

import           Foreign.C.Types (CInt)
import           Linear.V2       (V2)

import qualified SDL

class RenderEnv m where
  getWindow :: m SDL.Window
  getWindowSize :: m (V2 CInt)
  withRenderer :: (SDL.Renderer -> IO a) -> m a
  renderTexture :: SDL.Texture -> SDL.Rectangle CInt -> m ()
  -- drawRect
  -- fillRect

class HasEvent m where
  getEvents :: m [SDL.EventPayload]
