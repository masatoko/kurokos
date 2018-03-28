{-# LANGUAGE RecordWildCards #-}
module Kurokos.UI.Update
  ( updateGui
  ) where

import           Data.List.Extra           (firstJust)
import           Control.Lens
import           Control.Monad              (foldM)
import           Control.Monad.State
import           Data.Int                   (Int32)
import           Data.Maybe                 (catMaybes, mapMaybe, maybeToList)
import           Linear.V2
import           Safe                       (headMay, lastMay)

import           Kurokos.UI.Control
import           Kurokos.UI.Control.Control (wtTopmostAt)
import           Kurokos.UI.Core
import           Kurokos.UI.Event
import qualified Kurokos.UI.Event           as E
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget
import qualified Kurokos.UI.Widget.Update   as WU
import qualified Kurokos.UI.WidgetTree      as WT

import qualified SDL
import           SDL.Event

-- | Update Gui data by SDL Events. Call this at the top of Update
updateGui :: (RenderEnv m, MonadIO m) => [SDL.EventPayload] -> Cursor -> GUI -> m GUI
updateGui es cursor g0 = do
  g1 <- foldM (procEvent cursor) g0 es
  return $ handleGui es cursor g1

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

handleGui :: [SDL.EventPayload] -> Cursor -> GUI -> GUI
handleGui esSDL cursor gui =
  let es = case firstJust ghClick esSDL of
            Nothing  -> []
            Just act -> maybeToList $ clickEvent act cursor
  in gui&unGui._2.gstEvents .~ es
  where
    GuiHandler{..} = gui^.unGui._2.gstGuiHandler

    clickEvent :: GuiAction -> Cursor -> Maybe E.GuiEvent
    clickEvent act Cursor{..} = me
      where
        me = conv =<< wtTopmostAt _cursorPos isClickable (gui^.unGui._2.gstWTree)
          where
            isClickable = view (_1.ctxAttrib.clickable)
            conv (WContext{..}, w)
              | _ctxAttrib^.clickable = Just $ E.Clicked w _ctxIdent _ctxName _cursorPos act
              | otherwise             = Nothing

isWithinRect :: Point V2 CInt -> Point V2 CInt -> V2 CInt -> Bool
isWithinRect p p1 size =
  p1^._x <= px && px <= p2^._x && p1^._y <= py && py <= p2^._y
  where
    px = p^._x
    py = p^._y
    p2 = p1 + P size
