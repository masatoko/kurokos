{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Update where

import           Control.Lens
import           Control.Monad       (foldM)
import           Control.Monad.State
import           Data.Int            (Int32)
import           Data.Maybe          (catMaybes)
import           Linear.V2

import           Kurokos.GUI.Core
import           Kurokos.GUI.Event
import           Kurokos.GUI.Import
import           Kurokos.GUI.Types
import           Kurokos.GUI.Widget
import qualified Kurokos.GUI.WidgetTree as WT

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
        modify $ over gWTree $ mapWTPos (modWhenHover (fromIntegral <$> mouseMotionEventPos)) (pure 0)
      where
        modWhenHover curPos pos0 a@(ctx,w)
          | hoverable w && not (wst^.hover) && isWithinRect curPos pos size =
            let ctx' = ctx & ctxWidgetState . hover .~ True
                           & ctxNeedsRender .~ True
                           & ctxColor .~ (colorSetHover `modColor` colorSetBasis)
            in (ctx',w)
          | hoverable w && wst^.hover && not (isWithinRect curPos pos size) =
            let ctx' = ctx & ctxWidgetState . hover .~ False
                           & ctxNeedsRender .~ True
                           & ctxColor .~ colorSetBasis
            in (ctx',w)
          | otherwise = a
          where
            wst = ctx^.ctxWidgetState
            ti = ctx^.ctxTextureInfo
            pos = pos0 + (ti^.tiPos)
            size = ti^.tiSize
            ColorSet{..} = ctx^.ctxColorSet

    work (MouseButtonEvent MouseButtonEventData{..}) =
      return $ gui & gEvents %~ (es ++)
      where
        ws = filterAt mouseButtonEventPos $ gui^.gWTree
        et = SelectEvent mouseButtonEventMotion
        es = map conv ws
          where
            conv (ctx,w) = GuiEvent et w k mn
              where
                k = ctx^.ctxKey
                mn = ctx^.ctxIdent

    work _ = return gui

type ModifiableWidgetState = (ColorSet, WidgetColor, Bool, WidgetState, Widget)

filterAt :: Point V2 Int32 -> GuiWidgetTree -> [(WContext, Widget)]
filterAt aPos' = catMaybes . WT.toList . mapWTPos work (pure 0)
  where
    aPos = fromIntegral <$> aPos'

    work :: GuiPos -> (WContext, Widget) -> Maybe (WContext, Widget)
    work pos0 (ctx, w)
      | isWithinRect aPos pos (ti^.tiSize) = Just (ctx, w)
      | otherwise                          = Nothing
      where
        ti = ctx^.ctxTextureInfo
        pos = pos0 + (ti^.tiPos)

isWithinRect :: Point V2 CInt -> Point V2 CInt -> V2 CInt -> Bool
isWithinRect p p1 size =
  p1^._x <= px && px <= p2^._x && p1^._y <= py && py <= p2^._y
  where
    px = p^._x
    py = p^._y
    p2 = p1 + P size


-- update Widget by ident with function
updateByIdent :: WidgetIdent -> (Widget -> Widget) -> GUI -> GUI
updateByIdent wid f = over gWTree (fmap work)
  where
    work a@(ctx, w)
      | pIdent    = (ctx', f w)
      | otherwise = a
      where
        pIdent = ctx^.ctxIdent == Just wid
        ctx' = ctx & ctxNeedsRender .~ True
