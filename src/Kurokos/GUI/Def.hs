module Kurokos.GUI.Def where

import           Foreign.C.Types (CInt)

import qualified SDL

class RenderEnv m where
  withRenderer :: (SDL.Renderer -> m a) -> m a
  renderTexture :: SDL.Texture -> SDL.Rectangle CInt -> m ()
  -- drawRect
  -- fillRect
