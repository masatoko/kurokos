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

-- update by name with function
update :: WTName -> (CtxWidget -> CtxWidget) -> GUI -> GUI
update name f = over (unGui._2.gstWTree) (fmap work)
  where
    work a@(ctx,_)
      | ctx^.ctxName == Just name =
          let (ctx', w') = f a
          in (ctx' & ctxNeedsRender .~ True, w')
      | otherwise = a

findByIdent :: WTIdent -> GUI -> Maybe CtxWidget
findByIdent ident g = find isTarget $ g^.unGui._2.gstWTree
  where
    isTarget (ctx,_) = ctx^.ctxIdent == ident

findByName :: WTName -> GUI -> Maybe CtxWidget
findByName name g = find isTarget $ g^.unGui._2.gstWTree
  where
    isTarget (ctx,_) = ctx^.ctxName == Just name

-- | Set position of a widget directly
--
-- Use this when put a widget on the required position in world space.
setPositionInWorld :: WTName -> GuiPos -> GUI -> GUI
setPositionInWorld name g' = over (unGui._2.gstWTree) (fmap work)
  where
    work :: CtxWidget -> CtxWidget
    work a@(ctx, w)
      | ctx^.ctxName == Just name = setNeedsLayout (ctx', w)
      | otherwise                 = a
      where
        parent = g - l
          where
            g = ctx^.ctxWidgetState . wstWorldPos
            l = ctx^.ctxWidgetState . wstPos
        P (V2 x y) = g' - parent
        ctx' = ctx & ctxUPos .~ V2 (EConst x) (EConst y)
