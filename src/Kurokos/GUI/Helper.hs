{-# LANGUAGE FlexibleContexts #-}
module Kurokos.GUI.Helper where

import           Debug.Trace         (traceM)

import           Control.Lens
import           Control.Monad.State
import           Data.List           (find)
import           Safe                (headMay)

import qualified SDL

import           Kurokos.GUI.Core
import           Kurokos.GUI.Event
import           Kurokos.GUI.Import
import           Kurokos.GUI.Types
import           Kurokos.GUI.Widget  (Widget)

onClick :: Monad m => WidgetIdent -> (GuiEvent -> StateT GUI m a) -> StateT GUI m (Maybe a)
onClick wid act = do
  es <- use gEvents
  case find isTarget es of
    Just e  -> Just <$> act e
    Nothing -> return Nothing
  where
    isTarget e =
      geWidgetName e == Just wid
        && seInputMotion (geType e) == SDL.Pressed

-- update by ident with function
update :: WidgetIdent -> (CtxWidget -> CtxWidget) -> GUI -> GUI
update wid f = over gWTree (fmap work)
  where
    work a@(ctx,_)
      | ctx^.ctxIdent == Just wid =
          let (ctx', w') = f a
          in (ctx' & ctxNeedsRender .~ True, w')
      | otherwise = a

setGlobalPosition :: WidgetIdent -> GuiPos -> GUI -> GUI
setGlobalPosition wid global = over gWTree (mapWTPos work)
  where
    work :: GuiPos -> GuiPos -> CtxWidget -> CtxWidget
    work p0 _ a@(ctx, w)
      | ctx^.ctxIdent == Just wid = (ctx', w)
      | otherwise                 = a
      where
        local = global - p0
        ctx' = ctx & ctxWidgetState . wstPos .~ local
