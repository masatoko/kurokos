{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Kurokos.UI.Update
  ( updateGui
  ) where

import Debug.Trace (traceM)

import           Control.Lens
import           Control.Monad              (foldM)
import           Control.Monad.Extra        (whenJust)
import           Control.Monad.State
import           Data.Int                   (Int32)
import Data.List (foldl')
import           Data.List.Extra            (firstJust)
import           Data.Maybe                 (catMaybes, isJust, mapMaybe,
                                             maybeToList)
import qualified Data.Text.Zipper           as TZ
import           Linear.V2
import           Linear.Vector              ((^*))
import           Safe                       (atMay, headMay, lastMay)

import           Kurokos.UI.Control
import qualified Kurokos.UI.Control.Control as C
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
import           SDL.Event
import qualified SDL.Raw.Types

import           Foreign.Ptr
import qualified SDL.Raw.Event              as Raw

-- | Update Gui data by SDL Events. Call this at the top of Update
updateGui :: (RenderEnv m, MonadIO m) => [SDL.EventPayload] -> Cursor -> GUI -> m GUI
updateGui es cursor g0 = do
  let g1 = g0&unGui._2.gstEvents .~ [] -- Clear all events
  g2 <- foldM (procEvent cursor) g1 es
  -- liftIO $ do
  --   putStrLn "==="
  --   print =<< Raw.getMouseState nullPtr nullPtr
  mouseButtons <- SDL.getMouseButtons
  -- liftIO $
  --   print $ map (mouseButtons . SDL.ButtonExtra) [0..10]
  --   print $ mouseButtons SDL.ButtonRight
  return $ updateByGuiEvents $ handleGui mouseButtons es cursor prevEvents g2
  where
    prevEvents = g0^.unGui._2.gstEvents

procEvent :: (RenderEnv m, MonadIO m)
  => Cursor -> GUI -> SDL.EventPayload -> m GUI
procEvent cursor gui0 = work
  where
    curPos = cursor^.cursorPos
    isClickable = view (_1.ctxAttrib.clickable)

    -- resetFocus :: StateT GUI m ()
    resetFocus = do
      -- SDL.stopTextInput
      g <- get
      -- * Make GuiEvents for previous focused CtxWidget
      whenJust (WT.wtAt (g^.unGui._2.gstFocus) (g^.unGui._2.gstWTree)) $ \cw -> do
        let wif = cwToInfo cw
        unGui._2.gstEvents %= (E.Unfocused wif:)
        case cw^._2 of
          (TextField _ _ z _) -> do
            let textFixed = E.TextFixed (cwToInfo cw) (TZ.currentLine z)
            unGui._2.gstEvents %= (textFixed:)
          (Picker ts _ _ idx _) ->
            whenJust (ts `atMay` idx) $ \(key, text) ->
              unGui._2.gstEvents %= (PickerPicked wif key text :)
          _ -> return ()
      -- * Reset flags about focus
      modify $ \g -> g&unGui._2.gstFocus .~ []
      modify $ \g -> g&unGui._2.gstWTree %~ fmap (set (_1.ctxWidgetState.wstFocus) False)
    --
    work (WindowResizedEvent WindowResizedEventData{..}) = do
      win <- getWindow
      return $ if windowResizedEventWindow == win
                then setAllNeedsRender gui0
                else gui0
    work (WindowMaximizedEvent WindowMaximizedEventData{..}) = do
      win <- getWindow
      return $ if windowMaximizedEventWindow == win
                then setAllNeedsRender gui0
                else gui0
    work (TextInputEvent TextInputEventData{..}) =
      return $ C.modifyFocused work gui0
      where
        work cw = setNeedsResize $ cw & _2 %~ WM.widgetInputText textInputEventText
    work (KeyboardEvent KeyboardEventData{..}) =
      flip execStateT gui0 $
        when pressed modCursor
      where
        pressed = keyboardEventKeyMotion == Pressed
        scancode = SDL.keysymScancode keyboardEventKeysym
        modCursor =
          case scancode of
            SDL.ScancodeLeft      -> modify $ C.modifyFocused (C.modifyWidget WM.widgetLeft)
            SDL.ScancodeRight     -> modify $ C.modifyFocused (C.modifyWidget WM.widgetRight)
            SDL.ScancodeDelete    -> modify $ C.modifyFocused (setNeedsResize . C.modifyWidget WM.widgetDeleteChar)
            SDL.ScancodeBackspace -> modify $ C.modifyFocused (setNeedsResize . C.modifyWidget WM.widgetBackspace)
            SDL.ScancodeReturn    -> resetFocus >> SDL.stopTextInput
            _                     -> return ()
    work (MouseButtonEvent MouseButtonEventData{..}) =
      execStateT modWhenClicked gui0
      where
        clickedByLeft = mouseButtonEventButton == ButtonLeft
                          && mouseButtonEventMotion == Pressed
        modWhenClicked
          | clickedByLeft = do
              gui <- get
              case C.topmostAtWith curPos isClickable gui of
                Nothing -> return ()
                Just cw@(ctx,w) -> do
                  -- * Modify widget
                  let focusPath = ctx^.ctxPath
                  unGui._2.gstWTree %= WT.wtModifyAt focusPath conv
                  -- * Reset focus if need
                  unless (ctx^.ctxWidgetState.wstFocus) $ do -- When focus is changed
                    resetFocus
                    unGui._2.gstEvents %= (E.Focused (cwToInfo cw):)
                  -- * text input
                  case w of
                    TextField{} -> SDL.startTextInput $ SDL.Raw.Types.Rect 0 0 1000 1000
                    _           -> SDL.stopTextInput
                  -- * Focus flags
                  unGui._2.gstWTree %= WT.wtModifyAt focusPath (set (_1.ctxWidgetState.wstFocus) True)
                  unGui._2.gstFocus .= focusPath -- Call `resetFocus` before this
          | otherwise = return ()
          where
            conv cw@(ctx,w) =
              (ctx', WU.modifyOnClicked ctx curPos pos size w)
              where
                ctx' = ctx & ctxNeedsRender .~ True
                pos = ctx^.ctxWidgetState.wstWorldPos
                size = clickableSize cw
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
            size = clickableSize a
        modWhenHoverWithLHold a@(ctx,w)
          | ButtonLeft `elem` mouseMotionEventState && isWithinRect curPos pos size =
            let ctx' = ctx & ctxNeedsRender .~ True
            in (ctx', WU.modifyWhenHoverWithLHold curPos pos size w)
          | otherwise = a
          where
            pos = ctx^.ctxWidgetState.wstWorldPos
            size = clickableSize a
    work (MouseWheelEvent MouseWheelEventData{..}) =
      execStateT work gui0
      where
        delta = fromIntegral <$> mouseWheelEventPos ^* 10
        work = do
          gui <- get
          whenJust (C.topmostAtWith curPos isScrollableContainer gui) $ \(ctx,_) ->
            unGui._2.gstWTree %= WT.wtModifyAt (ctx^.ctxPath) (scrollContainer delta)

    work _ = return gui0

handleGui :: (SDL.MouseButton -> Bool) -> [SDL.EventPayload] -> Cursor -> [E.GuiEvent] -> GUI -> GUI
handleGui mouseButtons esSDL cursor prevEvents gui =
  gui&unGui._2.gstEvents %~ ((clickEvents ++ draggingEvents) ++)
  where
    actsClick = mapMaybe ghClick esSDL
    curPos = cursor^.cursorPos

    GuiHandler{..} = gui^.unGui._2.gstGuiHandler

    isClickable = view (_1.ctxAttrib.clickable)
    isDraggable = view (_1.ctxAttrib.draggable)
    isDroppable = view (_1.ctxAttrib.droppable)

    clickEvents :: [E.GuiEvent]
    clickEvents =
      maybe [] conv $ C.wtTopmostAt curPos isClickable (gui^.unGui._2.gstWTree)
      where
        conv cw@(WContext{..}, w)
          | _ctxAttrib^.clickable = map (E.Clicked (cwToInfo cw) curPos) actsClick
          | otherwise             = []

    draggingEvents =
      let drg = mapMaybe beginDrg esSDL ++ mapMaybe fromDrg prevEvents
          scr = mapMaybe beginScroll esSDL ++ mapMaybe fromScrl prevEvents
      in drg ++ scr
      where
        btn = ButtonExtra 0 -- FIX: SDL.getMouseButtons ButtonLeft doesn't work
        wt = gui^.unGui._2.gstWTree

        beginDrg (MouseButtonEvent MouseButtonEventData{..})
          | clickedByLeft = do
              cw <- C.wtTopmostAt curPos isDraggable wt
              return $ Dragging (cwToInfo cw) btn 0
          | otherwise = Nothing
          where
            clickedByLeft = mouseButtonEventButton == ButtonLeft
                              && mouseButtonEventMotion == Pressed
        beginDrg _ = Nothing

        fromDrg e@Dragging{}
          | held      = Just $ e {geCount = geCount e + 1}
          | not held  =
              let mInfo = case C.wtTopmostAt curPos isDroppable wt of
                    Nothing -> Nothing
                    Just cw -> Just $ cwToInfo cw
              in Just $ DragAndDrop (geWidgetInfo e) mInfo (geButton e)
          | otherwise = Nothing
          where
            held = mouseButtons $ geButton e
        fromDrg _ = Nothing

        beginScroll (MouseButtonEvent MouseButtonEventData{..})
          | clickedByLeft = do
              cw <- C.wtTopmostAt curPos (const True) wt
              if isScrollableContainer cw
                then Just $ ScrollingByDrag (cwToInfo cw) btn curPos (pure 0) 0
                else Nothing
          | otherwise     = Nothing
          where
            clickedByLeft = mouseButtonEventButton == ButtonLeft
                              && mouseButtonEventMotion == Pressed
        beginScroll _ = Nothing
        fromScrl e@ScrollingByDrag{}
          | held      =
            let P move = curPos - gePosition e
            in Just $ e {gePosition = curPos, geMove = move, geCount = geCount e + 1}
          | not held  = Nothing -- TODO: Make ScrollFixed event
          | otherwise = Nothing
          where
            held = mouseButtons $ geButton e
        fromScrl _ = Nothing

updateByGuiEvents :: GUI -> GUI
updateByGuiEvents gui0 =
  foldl' update gui0 es
  where
    es = gui0^.unGui._2.gstEvents

    update gui = go
      where
        go ScrollingByDrag{..} =
          gui & unGui._2.gstWTree %~ WT.wtModifyAt path (scrollContainer (fromIntegral <$> geMove))
          where
            path = wifPath geWidgetInfo

cwToInfo :: CtxWidget -> E.WidgetInfo
cwToInfo (WContext{..}, w) = E.WidgetInfo w _ctxIdent _ctxName _ctxPath

isWithinRect :: Point V2 CInt -> Point V2 CInt -> V2 CInt -> Bool
isWithinRect p p1 size =
  p1^._x <= px && px <= p2^._x && p1^._y <= py && py <= p2^._y
  where
    px = p^._x
    py = p^._y
    p2 = p1 + P size

isScrollableContainer :: CtxWidget -> Bool
isScrollableContainer cw = isScrollable && isContainer
  where
    isScrollable = cw^._1.ctxAttrib.scrollable
    isContainer = isJust $ cw^._1.ctxContainerType

scrollContainer :: V2 CInt -> CtxWidget -> CtxWidget
scrollContainer (V2 dx dy) cw = setNeedsResize $
  cw&_1.ctxWidgetState.wstShift._x %~ workX
    &_1.ctxWidgetState.wstShift._y %~ workY
  where
    V2 width height = wstSize $ cw^._1.ctxWidgetState
    V2 mMinWidth mMinHeight = cw^._1.ctxWidgetState.wstMinSize
    maxW = max 0 $ maybe 0 (+ (-width)) mMinWidth
    maxH = max 0 $ maybe 0 (+ (-height)) mMinHeight
    workX x = max (-maxW) . min 0 $ x + dx
    workY y = max (-maxH) . min 0 $ y + dy
