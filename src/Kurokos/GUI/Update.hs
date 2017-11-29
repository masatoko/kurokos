{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Update where

import           Control.Lens
import           Control.Monad        (foldM)
import           Control.Monad.State
import           Data.Int             (Int32)
import           Linear.V2

import           Kurokos.GUI.Core
import           Kurokos.GUI.Event
import           Kurokos.GUI.Import
import           Kurokos.GUI.Types
import           Kurokos.GUI.Widget

import qualified SDL
import           SDL.Event

update :: (RenderEnv m, HasEvent m, MonadIO m, MonadMask m) => GUI -> m GUI
update g0 = do
  es <- getEvents
  let g = g0 & gEvents .~ []
  foldM procEvent g es

procEvent :: (RenderEnv m, HasEvent m, MonadIO m, MonadMask m)
  => GUI -> SDL.EventPayload -> m GUI
procEvent gui = work
  where
    work (WindowResizedEvent WindowResizedEventData{..}) = do
      win <- getWindow
      if windowResizedEventWindow == win
        then return $ setAllNeedsRender gui
        else return gui
    work (MouseMotionEvent MouseMotionEventData{..}) =
      return . flip execState gui $ do
        if SDL.ButtonLeft `elem` mouseMotionEventState
          then gCursorTrajectory %= (mouseMotionEventPos:)
          else gCursorTrajectory .= []
        modify $ modifyAt mouseMotionEventPos go
      where
        go wt@Single{} =
          wt { wtNeedsRender = True
             , wtColor = colorSetHover `modColor` colorSetBasis
             }
          where
            ColorSet{..} = wtColorSet wt
        go wt = wt

    work (MouseButtonEvent MouseButtonEventData{..}) =
      return $ gui & gEvents %~ (es ++)
      where
        ws = findAt mouseButtonEventPos gui
        et = SelectEvent mouseButtonEventMotion
        es = map (\(w,k,mn) -> GuiEvent et w k mn) ws

    work _ = return gui

modifyAt :: Point V2 Int32 -> (WidgetTree -> WidgetTree) -> GUI -> GUI
modifyAt aPos' f =
  over gWTrees (map (work (pure 0)))
  where
    aPos = fromIntegral <$> aPos'
    work gPos0 wt@Single{..}
      | pWithin   = f wt
      | otherwise = wt
      where
        pWithin = isWithinRect aPos (gPos0 + tiPos) tiSize
        TextureInfo{..} = wtTexInfo
    work gPos0 wt@Container{..} = wt {wtChildren = wt'}
      where
        pos = gPos0 + tiPos
        TextureInfo{..} = wtTexInfo
        wt' = if isWithinRect aPos pos tiSize
                then map (work pos) wtChildren
                else wtChildren

findAt :: Point V2 Int32 -> GUI -> [(Widget, WTKey, Maybe String)]
findAt aPos' gui =
  concatMap (work (pure 0)) $ gui^.gWTrees
  where
    aPos = fromIntegral <$> aPos'
    work gPos0 Single{..} =
      [(wtWidget, wtKey, wtName) | isWithinRect aPos (gPos0 + tiPos) tiSize]
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


-- update Widget by ident with function
updateW :: WidgetIdent -> (Widget -> Widget) -> GUI -> GUI
updateW wid f = over gWTrees (map work)
  where
    work wt@Single{..}
      | wtName == Just wid = wt {wtNeedsRender = True, wtWidget = f wtWidget}
      | otherwise          = wt
    work wt@Container{..} = wt {wtChildren = map work wtChildren}
