module Kurokos.GUI.Update where

import Control.Monad (foldM)

import           Kurokos.GUI.Import
import           Kurokos.GUI.Core

import qualified SDL

update :: (RenderEnv m, HasEvent m, MonadIO m, MonadMask m) => GUI -> m GUI
update gui = foldM workEvent gui =<< getEvents

workEvent :: (RenderEnv m, HasEvent m, MonadIO m, MonadMask m) => GUI -> SDL.EventPayload -> m GUI
workEvent gui e =
  case e of
   (SDL.WindowResizedEvent dt) -> doWhenResized dt
   _ -> return gui
  where
    doWhenResized dt = do
      win <- getWindow
      when (SDL.windowResizedEventWindow dt == win) $ do
        resetTexture gui
        updateTexture gui
      return gui
