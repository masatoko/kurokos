module Kurokos.GUI.Update where

import           Kurokos.GUI.Import
import           Kurokos.GUI.Core

import qualified SDL

update :: (RenderEnv m, HasEvent m, MonadIO m, MonadMask m) => GUI -> m ()
update gui = do
  es <- getEvents
  win <- getWindow
  let pWinResized = any (isWinResized win) es
  when pWinResized $ do
    resetTexture gui
    updateTexture gui
  where
    isWinResized curWin (SDL.WindowResizedEvent dt) = SDL.windowResizedEventWindow dt == curWin
    isWinResized _ _ = False
