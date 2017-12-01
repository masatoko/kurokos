module Kurokos.GUI.Helper where

import Debug.Trace (traceM)

import Safe (headMay)
import Control.Lens
import Control.Monad.State

import qualified SDL

import Kurokos.GUI.Import
import Kurokos.GUI.Types
import Kurokos.GUI.Core
import Kurokos.GUI.Event
import           Kurokos.GUI.Widget (Widget)

onClick :: Monad m => WidgetIdent -> StateT GUI m a -> StateT GUI m (Maybe a)
onClick wid act = do
  es <- use gEvents
  if not . null $ filter isTarget es
    then Just <$> act
    else return Nothing
  where
    isTarget e =
      geWidgetName e == Just wid
        && geType e == SelectEvent SDL.Pressed

-- update by ident with function
update :: WidgetIdent -> ((WContext, Widget) -> (WContext, Widget)) -> GUI -> GUI
update wid f = over gWTree (fmap work)
  where
    work a@(ctx,_)
      | ctx^.ctxIdent == Just wid =
          let (ctx', w') = f a
          in (ctx' & ctxNeedsRender .~ True, w')
      | otherwise = a
