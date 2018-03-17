{-# LANGUAGE RecordWildCards #-}
module Kurokos.UI.Update
  ( updateGui
  ) where

import           Control.Lens
import           Control.Monad          (foldM)
import           Control.Monad.State
import           Data.Int               (Int32)
import           Data.Maybe             (catMaybes, mapMaybe, maybeToList)
import           Linear.V2
import           Safe                   (lastMay)

import           Kurokos.UI.Core
import           Kurokos.UI.Event
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget
import qualified Kurokos.UI.WidgetTree as WT
import qualified Kurokos.UI.Widget.Update as WU

import qualified SDL
import           SDL.Event

-- | Update Gui data by SDL Events. Call this at the top of Update
updateGui :: (RenderEnv m, MonadIO m) => [SDL.EventPayload] -> Cursor -> GUI -> m GUI
updateGui es cursor g0 = foldM (procEvent cursor) g0 es

procEvent :: (RenderEnv m, MonadIO m)
  => Cursor -> GUI -> SDL.EventPayload -> m GUI
procEvent cursor gui = work
  where
    curPos = cursor^.cursorPos
    --
    work (WindowResizedEvent WindowResizedEventData{..}) = do
      win <- getWindow
      return $ if windowResizedEventWindow == win
                then setAllNeedsLayout . setAllNeedsRender $ gui
                else gui
    work (WindowMaximizedEvent WindowMaximizedEventData{..}) = do
      win <- getWindow
      return $ if windowMaximizedEventWindow == win
                then setAllNeedsLayout . setAllNeedsRender $ gui
                else gui
    work (MouseButtonEvent MouseButtonEventData{..}) =
      return . flip execState gui $
        modify $ over (unGui._2.gstWTree) (fmap modWhenClicked)
      where
        clickedByLeft = mouseButtonEventButton == ButtonLeft
                          && mouseButtonEventMotion == Pressed
        modWhenClicked a@(ctx,w)
          | clickedByLeft && isWithinRect curPos pos size =
            let ctx' = ctx & ctxNeedsRender .~ True
            in (ctx', WU.modifyOnClicked curPos pos size w)
          | otherwise = a
          where
            pos = ctx^.ctxWidgetState.wstWorldPos
            size = wstSize $ ctx^.ctxWidgetState
    work (MouseMotionEvent MouseMotionEventData{..}) =
      return . flip execState gui $ do
        modify $ over (unGui._2.gstWTree) (fmap modWhenHover)
        modify $ over (unGui._2.gstWTree) (fmap modWhenHoverWithLHold)
      where
        modWhenHover a@(ctx,w)
          | isHoverable && not (wst^.wstHover) && isWithinRect curPos pos size =
            let ctx' = ctx & ctxWidgetState . wstHover .~ True
                           & ctxNeedsRender .~ True
            in (ctx',w)
          | isHoverable && wst^.wstHover && not (isWithinRect curPos pos size) =
            let ctx' = ctx & ctxWidgetState . wstHover .~ False
                           & ctxNeedsRender .~ True
            in (ctx',w)
          | otherwise = a
          where
            pos = ctx^.ctxWidgetState.wstWorldPos
            isHoverable = ctx^.ctxAttrib.hoverable
            wst = ctx^.ctxWidgetState
            size = wstSize wst
        modWhenHoverWithLHold a@(ctx,w)
          | ButtonLeft `elem` mouseMotionEventState && isWithinRect curPos pos size =
            let ctx' = ctx & ctxNeedsRender .~ True
            in (ctx', WU.modifyWhenHoverWithLHold curPos pos size w)
          | otherwise = a
          where
            pos = ctx^.ctxWidgetState.wstWorldPos
            size = wstSize $ ctx^.ctxWidgetState

    work _ = return gui

isWithinRect :: Point V2 CInt -> Point V2 CInt -> V2 CInt -> Bool
isWithinRect p p1 size =
  p1^._x <= px && px <= p2^._x && p1^._y <= py && py <= p2^._y
  where
    px = p^._x
    py = p^._y
    p2 = p1 + P size
