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

import Foreign.Ptr
import qualified SDL.Raw.Event as Raw

-- | Update Gui data by SDL Events. Call this at the top of Update
updateGui :: (RenderEnv m, MonadIO m) => [SDL.EventPayload] -> Cursor -> GUI -> m GUI
updateGui es cursor g0 = do
  g1 <- foldM (procEvent cursor) g0 es
  -- liftIO $ do
  --   putStrLn "==="
  --   print =<< Raw.getMouseState nullPtr nullPtr
  mouseButtons <- SDL.getMouseButtons
  -- liftIO $
  --   print $ map (mouseButtons . SDL.ButtonExtra) [0..10]
  --   print $ mouseButtons SDL.ButtonRight
  return $ handleGui mouseButtons es cursor g1

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

handleGui :: (SDL.MouseButton -> Bool) -> [SDL.EventPayload] -> Cursor -> GUI -> GUI
handleGui mouseButtons esSDL Cursor{..} gui =
  gui&unGui._2.gstEvents .~ (clickEvents ++ draggingEvents)
  where
    actsClick = mapMaybe ghClick esSDL

    GuiHandler{..} = gui^.unGui._2.gstGuiHandler

    isClickable = view (_1.ctxAttrib.clickable)
    isDraggable = isClickable -- TODO: Add field
    isDroppable = isClickable -- TODO: Add field

    cwToInfo (WContext{..}, w) = E.WidgetInfo w _ctxIdent _ctxName

    clickEvents :: [E.GuiEvent]
    clickEvents =
      maybe [] conv $ wtTopmostAt _cursorPos isClickable (gui^.unGui._2.gstWTree)
      where
        conv cw@(WContext{..}, w)
          | _ctxAttrib^.clickable = map (E.Clicked (cwToInfo cw) _cursorPos) actsClick
          | otherwise             = []

    -- dragEvents :: [E.GuiEvent]
    -- dragEvents =
    --   where
    --     work

    draggingEvents =
      let bgns = mapMaybe beginDrg esSDL
          cnts = mapMaybe fromDrg esPrev
      in bgns ++ cnts
      where
        esPrev = gui^.unGui._2.gstEvents
        btn = ButtonExtra 0 -- FIX: SDL.getMouseButtons ButtonLeft doesn't work
        wt = gui^.unGui._2.gstWTree

        beginDrg (MouseButtonEvent MouseButtonEventData{..})
          | clickedByLeft =
              case wtTopmostAt _cursorPos isClickable wt of
                Nothing -> Nothing
                Just cw -> Just $ Dragging (cwToInfo cw) btn 0
          | otherwise = Nothing
          where
            clickedByLeft = mouseButtonEventButton == ButtonLeft
                              && mouseButtonEventMotion == Pressed
        beginDrg _ = Nothing

        fromDrg e@Dragging{}
          | held      = Just $ e {geCount = geCount e + 1}
          | not held  =
              let mInfo = case wtTopmostAt _cursorPos isDroppable wt of
                    Nothing -> Nothing
                    Just cw -> Just $ cwToInfo cw
              in Just $ DragAndDrop (geWidgetInfo e) mInfo (geButton e)
          | otherwise = Nothing
          where
            held = mouseButtons $ geButton e
        fromDrg _ = Nothing

isWithinRect :: Point V2 CInt -> Point V2 CInt -> V2 CInt -> Bool
isWithinRect p p1 size =
  p1^._x <= px && px <= p2^._x && p1^._y <= py && py <= p2^._y
  where
    px = p^._x
    py = p^._y
    p2 = p1 + P size
