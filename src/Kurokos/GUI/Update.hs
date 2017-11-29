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
          then gDragTrajectory %= (mouseMotionEventPos:)
          else gDragTrajectory .= []
        modify $ modifyW (go (fromIntegral <$> mouseMotionEventPos))
      where
        go curPos pos size (cs@ColorSet{..}, _, _, wst, w)
          | hoverable w && not (wst^.hover) && isWithinRect curPos pos size =
            let wc' = colorSetHover `modColor` colorSetBasis
                wst' = wst & hover .~ True
            in Just (cs, wc', True, wst', w)
          | hoverable w && wst^.hover && not (isWithinRect curPos pos size) =
            let wst' = wst & hover .~ False
            in Just (cs, colorSetBasis, True, wst', w)
          | otherwise = Nothing
          where

    work (MouseButtonEvent MouseButtonEventData{..}) =
      return $ gui & gEvents %~ (es ++)
      where
        ws = findAt mouseButtonEventPos gui
        et = SelectEvent mouseButtonEventMotion
        es = map (\(w,k,mn) -> GuiEvent et w k mn) ws

    work _ = return gui

type ModifiableWidgetState = (ColorSet, WidgetColor, Bool, WidgetState, Widget)

modifyW :: (Point V2 CInt -> V2 CInt -> ModifiableWidgetState -> Maybe ModifiableWidgetState) -> GUI -> GUI
modifyW f =
  over gWTrees (map (work (pure 0)))
  where
    work pos0 wt@Single{..} =
      case f pos tiSize mws of
        Nothing -> wt
        Just (wtColorSet', wtColor', wtNeedsRender', wtWidgetState', wtWidget') ->
          wt { wtColorSet = wtColorSet'
             , wtColor = wtColor'
             , wtNeedsRender = wtNeedsRender'
             , wtWidgetState = wtWidgetState'
             , wtWidget = wtWidget'
             }
      where
        mws = (wtColorSet, wtColor, wtNeedsRender, wtWidgetState, wtWidget)
        pos = pos0 + tiPos
        TextureInfo{..} = wtTexInfo
    work pos0 wt@Container{..} =
      wt {wtChildren = map (work pos) wtChildren}
      where
        pos = pos0 + tiPos
        TextureInfo{..} = wtTexInfo

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
