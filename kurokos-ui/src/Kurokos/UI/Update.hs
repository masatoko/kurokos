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

import qualified SDL
import           SDL.Event

-- | Update Gui data by SDL Events
updateGui :: (RenderEnv m, MonadIO m, MonadMask m) => [SDL.EventPayload] -> GUI -> m GUI
updateGui es g0 = foldM procEvent g es
  where
    g = g0 & gEvents .~ []

procEvent :: (RenderEnv m, MonadIO m, MonadMask m)
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
        modify $ over gWTree $ fmap $ modWhenHover (fromIntegral <$> mouseMotionEventPos)
      where
        modWhenHover curPos a@(ctx,w)
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
            pos = ctx^.ctxWidgetState.wstGlobalPos
            isHoverable = ctx^.ctxAttrib.hoverable
            wst = ctx^.ctxWidgetState
            size = wst^.wstSize
            ColorSet{..} = ctx^.ctxColorSet

    -- Make SelectEvent
    work (MouseButtonEvent dt@MouseButtonEventData{..}) =
      return $ gui & gEvents %~ (es ++)
      where
        es = maybeToList $ conv =<< topmostAt mouseButtonEventPos (gui^.gWTree)
          where
            conv (ctx,w)
              | ctx^.ctxAttrib.clickable = Just $ GuiEvent et w k mn
              | otherwise                = Nothing
              where
                et = MouseClick dt
                k = ctx^.ctxKey
                mn = ctx^.ctxIdent

    work _ = return gui

topmostAt :: Point V2 Int32 -> GuiWidgetTree -> Maybe (WContext, Widget)
topmostAt p = lastMay . filterAt p

filterAt :: Point V2 Int32 -> GuiWidgetTree -> [(WContext, Widget)]
filterAt aPos' = catMaybes . WT.toList . fmap work
  where
    aPos = fromIntegral <$> aPos'

    work :: (WContext, Widget) -> Maybe (WContext, Widget)
    work cw
      | vis && within = Just cw
      | otherwise     = Nothing
      where
        wst = cw^._1.ctxWidgetState
        pos = wst^.wstGlobalPos
        size = wst^.wstSize
        --
        vis = wst^.wstVisible
        within = isWithinRect aPos pos size

isWithinRect :: Point V2 CInt -> Point V2 CInt -> V2 CInt -> Bool
isWithinRect p p1 size =
  p1^._x <= px && px <= p2^._x && p1^._y <= py && py <= p2^._y
  where
    px = p^._x
    py = p^._y
    p2 = p1 + P size
