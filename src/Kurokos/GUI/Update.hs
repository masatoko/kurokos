{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Update where

import           Control.Lens
import           Control.Monad        (foldM)
import           Control.Monad.Writer
import           Data.Int             (Int32)
import           Linear.V2

import           Kurokos.GUI.Core
import           Kurokos.GUI.Event
import           Kurokos.GUI.Import
import           Kurokos.GUI.Widget

import qualified SDL
import           SDL.Event

update :: (RenderEnv m, HasEvent m, MonadIO m, MonadMask m) => GUI -> m GUI
update g0 = do
  es <- getEvents
  let g = g0 & gEvents .~ []
  foldM procEvent g es

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
      let ws = findAt gui mouseButtonEventPos
          et = SelectEvent mouseButtonEventMotion
          es = map (GuiEvent et) ws
      return $ gui & gEvents %~ (es ++)

    work _ = return gui

findAt :: GUI -> Point V2 Int32 -> [Widget]
findAt gui aPos' =
  concatMap (work (pure 0)) $ gui^.gWTrees
  where
    aPos = fromIntegral <$> aPos'
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
