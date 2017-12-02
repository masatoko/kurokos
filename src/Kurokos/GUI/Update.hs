{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Update
  ( updateGui
  ) where

import           Control.Lens
import           Control.Monad       (foldM)
import           Control.Monad.State
import           Data.Int            (Int32)
import           Data.Maybe          (catMaybes, mapMaybe)
import           Linear.V2

import           Kurokos.GUI.Core
import           Kurokos.GUI.Event
import           Kurokos.GUI.Import
import           Kurokos.GUI.Types
import           Kurokos.GUI.Widget
import qualified Kurokos.GUI.WidgetTree as WT

import qualified SDL
import           SDL.Event

updateGui :: (RenderEnv m, HasEvent m, MonadIO m, MonadMask m) => GUI -> m GUI
updateGui g0 = do
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
        modify $ over gWTree $ mapWTPos $ modWhenHover (fromIntegral <$> mouseMotionEventPos)
      where
        modWhenHover curPos pos a@(ctx,w)
          | isHoverable && not (wst^.wstHover) && isWithinRect curPos pos size =
            let ctx' = ctx & ctxWidgetState . wstHover .~ True
                           & ctxNeedsRender .~ True
                           & ctxColor .~ (colorSetHover `modColor` colorSetBasis)
            in (ctx',w)
          | isHoverable && wst^.wstHover && not (isWithinRect curPos pos size) =
            let ctx' = ctx & ctxWidgetState . wstHover .~ False
                           & ctxNeedsRender .~ True
                           & ctxColor .~ colorSetBasis
            in (ctx',w)
          | otherwise = a
          where
            isHoverable = ctx^.ctxAttrib.hoverable
            wst = ctx^.ctxWidgetState
            size = wst^.wstSize
            ColorSet{..} = ctx^.ctxColorSet

    work (MouseButtonEvent MouseButtonEventData{..}) =
      return $ gui & gEvents %~ (es ++)
      where
        ws = filterAt mouseButtonEventPos $ gui^.gWTree
        et = SelectEvent mouseButtonEventMotion
        es = mapMaybe conv ws
          where
            conv (ctx,w)
              | ctx^.ctxAttrib.clickable = Just $ GuiEvent et w k mn
              | otherwise                = Nothing
              where
                k = ctx^.ctxKey
                mn = ctx^.ctxIdent

    work _ = return gui

filterAt :: Point V2 Int32 -> GuiWidgetTree -> [(WContext, Widget)]
filterAt aPos' = catMaybes . WT.toList . mapWTPos work
  where
    aPos = fromIntegral <$> aPos'

    work :: GuiPos -> (WContext, Widget) -> Maybe (WContext, Widget)
    work pos (ctx, w)
      | isWithinRect aPos pos size = Just (ctx, w)
      | otherwise                  = Nothing
      where
        size = ctx^.ctxWidgetState.wstSize

isWithinRect :: Point V2 CInt -> Point V2 CInt -> V2 CInt -> Bool
isWithinRect p p1 size =
  p1^._x <= px && px <= p2^._x && p1^._y <= py && py <= p2^._y
  where
    px = p^._x
    py = p^._y
    p2 = p1 + P size
