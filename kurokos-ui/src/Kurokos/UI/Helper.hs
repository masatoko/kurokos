{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Kurokos.UI.Helper where

import           Debug.Trace         (traceM)

import           Control.Lens
import           Control.Monad.State
import           Data.Foldable       (find)
import           Data.List           (find)
import           Data.List.Extra     (firstJust)
import           Safe                (headMay)

import qualified SDL
import           SDL.Event

import           Kurokos.UI.Core
import           Kurokos.UI.Event
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget  (Widget)

clicked :: WidgetIdent -> GUI -> Maybe (GuiPos, SDL.MouseButton)
clicked wid = firstJust isTarget . view gEvents
  where
    isTarget e
      | geWidgetName e == Just wid = posFrom $ geType e
      | otherwise                  = Nothing
      where
        posFrom (MouseClick MouseButtonEventData{..})
          | cond      = Just (fromIntegral <$> mouseButtonEventPos, mouseButtonEventButton)
          | otherwise = Nothing
          where
            cond = SDL.Pressed == mouseButtonEventMotion

-- update by ident with function
update :: WidgetIdent -> (CtxWidget -> CtxWidget) -> GUI -> GUI
update wid f = over gWTree (fmap work)
  where
    work a@(ctx,_)
      | ctx^.ctxIdent == Just wid =
          let (ctx', w') = f a
          in (ctx' & ctxNeedsRender .~ True, w')
      | otherwise = a

glookup :: WidgetIdent -> GUI -> Maybe CtxWidget
glookup wid = find isTarget . view gWTree
  where
    isTarget = (== Just wid) . view (_1 . ctxIdent)

setGlobalPosition :: WidgetIdent -> GuiPos -> GUI -> GUI
setGlobalPosition wid g' = over gWTree (fmap work)
  where
    work :: CtxWidget -> CtxWidget
    work a@(ctx, w)
      | ctx^.ctxIdent == Just wid = (ctx', w)
      | otherwise                 = a
      where
        parent = g - l
          where
            g = ctx^.ctxWidgetState . wstGlobalPos
            l = ctx^.ctxWidgetState . wstPos
        P (V2 x y) = g' - parent
        ctx' = ctx & ctxUPos .~ V2 (EConst x) (EConst y)
                   & ctxNeedsLayout .~ True
