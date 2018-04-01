{-# LANGUAGE RecordWildCards #-}
module Kurokos.UI.Update
  ( updateGui
  ) where

import           Control.Lens
import           Control.Monad              (foldM)
import           Control.Monad.State
import           Data.Int                   (Int32)
import           Data.List.Extra            (firstJust)
import           Data.Maybe                 (catMaybes, mapMaybe, maybeToList)
import           Linear.V2
import           Safe                       (headMay, lastMay)

import           Kurokos.UI.Control
import          qualified Kurokos.UI.Control.Control as C
import           Kurokos.UI.Core
import           Kurokos.UI.Event
import qualified Kurokos.UI.Event           as E
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget
import           Kurokos.UI.Widget.Module   as WM
import qualified Kurokos.UI.Widget.Update   as WU
import qualified Kurokos.UI.WidgetTree      as WT

import qualified SDL
import qualified SDL.Raw.Types
import           SDL.Event

import           Foreign.Ptr
import qualified SDL.Raw.Event              as Raw

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
procEvent cursor gui0 = work
  where
    curPos = cursor^.cursorPos
    isClickable = view (_1.ctxAttrib.clickable)
    --
    work (WindowResizedEvent WindowResizedEventData{..}) = do
      win <- getWindow
      return $ if windowResizedEventWindow == win
                then setAllNeedsLayout . setAllNeedsRender $ gui0
                else gui0
    work (WindowMaximizedEvent WindowMaximizedEventData{..}) = do
      win <- getWindow
      return $ if windowMaximizedEventWindow == win
                then setAllNeedsLayout . setAllNeedsRender $ gui0
                else gui0
    work (TextInputEvent TextInputEventData{..}) =
      return $ C.modifyFocused (over _2 (WM.widgetInputText textInputEventText)) gui0
    work (KeyboardEvent KeyboardEventData{..}) =
      return . flip execState gui0 $
        when pressed $ modify modCursor
      where
        pressed = keyboardEventKeyMotion == Pressed
        scancode = SDL.keysymScancode keyboardEventKeysym
        modCursor gui =
          case scancode of
            SDL.ScancodeLeft      -> C.modifyFocused (C.modifyWidget WM.widgetLeft) gui
            SDL.ScancodeRight     -> C.modifyFocused (C.modifyWidget WM.widgetRight) gui
            SDL.ScancodeDelete    -> C.modifyFocused (C.modifyWidget WM.widgetDeleteChar) gui
            SDL.ScancodeBackspace -> C.modifyFocused (C.modifyWidget WM.widgetBackspace) gui
            _                     -> gui
    work (MouseButtonEvent MouseButtonEventData{..}) =
      modWhenClicked gui0
      where
        clickedByLeft = mouseButtonEventButton == ButtonLeft
                          && mouseButtonEventMotion == Pressed
        modWhenClicked gui
          | clickedByLeft =
              case C.topmostAtWith curPos isClickable gui of
                Nothing -> return gui
                Just (ctx,w) -> do
                  let path = ctx^.ctxPath
                      gui' = gui & unGui._2.gstWTree %~ WT.wtModifyAt path conv
                                 & unGui._2.gstFocus .~ path
                  case w of
                    TextField{} -> SDL.startTextInput $ SDL.Raw.Types.Rect 100 100 100 100
                    _           -> SDL.stopTextInput
                  return gui'
          | otherwise = return gui
          where
            conv (ctx,w) =
              (ctx', WU.modifyOnClicked curPos pos size w)
              where
                ctx' = ctx & ctxNeedsRender .~ True
                pos = ctx^.ctxWidgetState.wstWorldPos
                size = wstSize $ ctx^.ctxWidgetState
    work (MouseMotionEvent MouseMotionEventData{..}) =
      return . flip execState gui0 $ do
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

    work _ = return gui0

handleGui :: (SDL.MouseButton -> Bool) -> [SDL.EventPayload] -> Cursor -> GUI -> GUI
handleGui mouseButtons esSDL Cursor{..} gui =
  gui&unGui._2.gstEvents .~ (clickEvents ++ draggingEvents)
  where
    actsClick = mapMaybe ghClick esSDL

    GuiHandler{..} = gui^.unGui._2.gstGuiHandler

    isClickable = view (_1.ctxAttrib.clickable)
    isDraggable = view (_1.ctxAttrib.draggable)
    isDroppable = view (_1.ctxAttrib.droppable)

    cwToInfo (WContext{..}, w) = E.WidgetInfo w _ctxIdent _ctxName

    clickEvents :: [E.GuiEvent]
    clickEvents =
      maybe [] conv $ C.wtTopmostAt _cursorPos isClickable (gui^.unGui._2.gstWTree)
      where
        conv cw@(WContext{..}, w)
          | _ctxAttrib^.clickable = map (E.Clicked (cwToInfo cw) _cursorPos) actsClick
          | otherwise             = []

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
              case C.wtTopmostAt _cursorPos isDraggable wt of
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
              let mInfo = case C.wtTopmostAt _cursorPos isDroppable wt of
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
