module Kurokos.GUI.Def where

import           Kurokos.GUI.Import

import qualified SDL

class RenderEnv m where
  getWindow :: m SDL.Window
  getWindowSize :: m (V2 CInt)
  withRenderer :: (SDL.Renderer -> IO a) -> m a
  renderTexture :: SDL.Texture -> SDL.Rectangle CInt -> m ()

class HasEvent m where
  getEvents :: m [SDL.EventPayload]
