{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kurokos.UI.File.Convert where

import           Control.Lens
import qualified Control.Exception.Safe  as E
import           Data.ByteString         (ByteString)
import qualified Data.Yaml               as Y

import           Kurokos.UI.Core
import           Kurokos.UI.Def
import           Kurokos.UI.Types
import           Kurokos.UI.File.Yaml   (YWidget (..), decodeWidgets)
import           Kurokos.UI.Import
import           Kurokos.UI.Widget
import           Kurokos.UI.WidgetTree
import           Kurokos.UI.Widget.Make

newWidgetTreeFromData :: (RenderEnv m, MonadIO m, MonadThrow m, MonadResource m)
  => ByteString -> GuiT m GuiWidgetTree
newWidgetTreeFromData bs =
  case decodeWidgets bs of
    Left e   -> E.throw e
    Right ys -> work ys
  where
    work ys = do
      ws <- mapM convert ys
      return $ mconcat ws

convert :: (RenderEnv m, MonadIO m, MonadThrow m, MonadResource m)
  => YWidget -> GuiT m GuiWidgetTree
convert s@Single{..} = do
  wt <- genSingle wIdent (V2 wX wY) (V2 wWidth wHeight) =<< genWidget wType
  return $ wt & wtElement._1 %~ setContext
  where
    title = fromMaybe " " wTitle

    genWidget "fill"   = newFill
    genWidget "label"  = getAssetId >>= \ident -> newLabel ident title
    genWidget "button" = getAssetId >>= \ident -> newButton ident title
    genWidget "image"  = newImageView =<< getAssetId
    genWidget wtype    = E.throwIO $ userError $ "unkown widget type: " ++ wtype

    getAssetId = case wAsset of
                  Nothing      -> E.throwIO $ userError $ "missing asset: " ++ show s
                  Just assetid -> return assetid

    setContext ctx =
      ctx & ctxAttrib . visible %~ flip fromMaybe wVisible
          & ctxAttrib . clickable %~ flip fromMaybe wClickable

convert Container{..} = do
  cnt <- genContainer wIdent wContainerType (V2 wX wY) (V2 wWidth wHeight)
  let cnt' = cnt & wtElement._1 %~ setContext
  ws <- mapM convert wChildren
  let w = mconcat ws
  return $ fromMaybe w $ prependChild cnt' w
  where
    setContext ctx =
      ctx & ctxAttrib . visible %~ flip fromMaybe wVisible
          & ctxAttrib . clickable %~ flip fromMaybe wClickable