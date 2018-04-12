module Kurokos.UI.Helper where

import           Debug.Trace           (traceM)

import qualified Control.Exception as E
import           Control.Lens
import           Control.Monad.State
import           Data.Foldable         (find)
import           Data.List             (find)
import           Data.List.Extra       (firstJust)
import           Safe                  (headMay)

import qualified SDL
import           SDL.Event

import           Kurokos.UI.Core
import           Kurokos.UI.Event
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget     (Widget)
import           Kurokos.UI.WidgetTree

prettyWT :: GuiWidgetTree -> String
prettyWT = prettyWith showWidget
  where
    showWidget (ctx,w) = show w ++ " - " ++ ctx'
      where
        ctx' = concat
          [ "#", show (unWidgetIdent $ ctx^.ctxIdent), " "
          , "'" , fromMaybe "-" (ctx^.ctxName), "' "
          , maybe "" show (ctx^.ctxContainerType)]

isNameOf :: WTName -> CtxWidget -> Bool
isNameOf name (ctx,_) = ctx^.ctxName == Just name

isIdentOf :: WTIdent -> CtxWidget -> Bool
isIdentOf ident (ctx,_) = ctx^.ctxIdent == ident

-- | Update targets with function
--
-- @
-- let gui' = UI.update (UI.isNameOf "target_name") (\cw -> cw&_1.ctxNeedsRender .~ True) gui
-- @
update :: (CtxWidget -> Bool) -> (CtxWidget -> CtxWidget) -> GUI -> GUI
update isTarget f gui = gui & unGui._2.gstWTree %~ fmap work
                            & unGui._2.gstUpdated .~ True
  where
    work a
      | isTarget a = f a
      | otherwise  = a

findByIdent :: WTIdent -> GUI -> Maybe CtxWidget
findByIdent ident g = find isTarget $ g^.unGui._2.gstWTree
  where
    isTarget (ctx,_) = ctx^.ctxIdent == ident

findByName :: WTName -> GUI -> Maybe CtxWidget
findByName name g = find isTarget $ g^.unGui._2.gstWTree
  where
    isTarget (ctx,_) = ctx^.ctxName == Just name

-- | Put a WidgetTree to container
--
-- @
-- makeChild :: MonadIO m => GuiT m PathMap
-- makeChild = do
--   wt <- makeGuiWidgetTree
--   UI.modifyRoot $ UI.putChildToContainer ("cntn-name" `UI.isNameOf`) wt
-- @
putChildToContainer :: MonadIO m => (CtxWidget -> Bool) -- ^ Parent container matching function
         -> GuiWidgetTree -- ^ New children
         -> GuiWidgetTree -- ^ Original GuiWidgetTree
         -> m GuiWidgetTree -- ^ New GuiWidgetTree
putChildToContainer isParent newCs orgTr =
  case focusBy isParent (toZipper orgTr) of
    Nothing          -> liftIO $ E.throwIO $ userError "Missing GuiWidgetTree @putChildToContainer"
    Just (tr,crumbs) ->
      case tr of
        Null            -> liftIO $ E.throwIO $ userError "Target tree is Null @putChildToContainer"
        (Fork u a mc o) ->
          case mc of
            Nothing -> liftIO $ E.throwIO $ userError "Target tree is not Container @putChildToContainer"
            Just c  -> do
              freeGuiWidgetTree c
              return $ fromZipper (Fork u a (Just newCs) o, crumbs)

-- | Set position of a widget directly
--
-- Use this when put a widget on the required position in world space.
-- Use with `update` @let gui' = UI.update (UI.isNameOf "name") (UI.setPositionInWorld (V2 x y)) gui@
setPositionInWorld :: GuiPos -> CtxWidget -> CtxWidget
setPositionInWorld wpos (ctx, w) = setNeedsResize (ctx', w)
  where
    parent = (ctx^.ctxWidgetState.wstWorldPos) - (ctx^.ctxWidgetState.wstLocalPos)
    P (V2 x y) = wpos - parent
    ctx' = ctx & ctxUPos .~ V2 (EConst x) (EConst y)
