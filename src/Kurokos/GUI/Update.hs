{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Update where

import Control.Lens
import           Control.Monad      (foldM)
import Linear.V2

import           Kurokos.GUI.Core
import           Kurokos.GUI.Import
import           Kurokos.GUI.Widget

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
    work (MouseButtonEvent MouseButtonEventData{..}) = do
      let ws = findAt gui (fromIntegral <$> mouseButtonEventPos)
      unless (null ws) $ liftIO . print $ ws
      return gui

    work _ = return gui

findAt :: GUI -> Point V2 CInt -> [Widget]
findAt gui aPos =
  concatMap (work (pure 0)) $ gui^.gWTrees
  where
    work gPos0 Single{..} =
      [wtWidget | isWithinRect aPos (gPos0 + tiPos) tiSize]
      where
        TextureInfo{..} = wtTexInfo
    work gPos0 Container{..} =
      if isWithinRect aPos pos tiSize
        then concatMap (work pos) wtChildren
        else []
      where
        pos = gPos0 + tiPos
        TextureInfo{..} = wtTexInfo

isWithinRect :: Point V2 CInt -> Point V2 CInt -> V2 CInt -> Bool
isWithinRect p p1 size =
  p1^._x <= px && px <= p2^._x && p1^._y <= py && py <= p2^._y
  where
    px = p^._x
    py = p^._y
    p2 = p1 + P size
