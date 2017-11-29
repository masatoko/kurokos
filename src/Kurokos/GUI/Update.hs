{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Update where

import           Control.Monad      (foldM)

import           Kurokos.GUI.Core
import           Kurokos.GUI.Import

import qualified SDL
import           SDL.Event

update :: (RenderEnv m, HasEvent m, MonadIO m, MonadMask m) => GUI -> m GUI
update gui = foldM procEvent gui =<< getEvents

procEvent :: (RenderEnv m, HasEvent m, MonadIO m, MonadMask m) => GUI -> SDL.EventPayload -> m GUI
procEvent gui = work
  where
    work (WindowResizedEvent WindowResizedEventData{..}) = do
      win <- getWindow
      when (windowResizedEventWindow == win) $ do
        resetTexture gui
        updateTexture gui
      return gui

    work _ = return gui
