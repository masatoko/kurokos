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
      if windowResizedEventWindow == win
        then do
          resetTexture gui
          updateTexture gui
        else return gui
    -- work (MouseButtonEvent MouseButtonEventData{..}) = do
    --   let ws = findAt gui mouseButtonEventPos
    --   return gui

    work _ = return gui

-- findAt :: GUI -> Point V2 CInt -> [Widget]
-- findAt gui (P pos) =
--   concatMap (work (pure 0)) $ gui^.gWTrees
--   where
--     work p0 wt = do
--       let localPos = p0 + tiPos
--       where
--         TextureInfo{..} = wtTexInfo wt

-- withinRect :: Point V2 CInt -> V2 CInt
