module Kurokos.GUI.Def where

import           Foreign.C.Types        (CInt)

import qualified SDL

class RenderEnv m where
  getWindow :: m SDL.Window
  withRenderer :: (SDL.Renderer -> IO a) -> m a
  renderTexture :: SDL.Texture -> SDL.Rectangle CInt -> m ()
  -- drawRect
  -- fillRect
