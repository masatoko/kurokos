{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kurokos.GUI.File.Convert where

import           Control.Lens
import qualified Control.Exception.Safe  as E
import           Data.ByteString         (ByteString)
import qualified Data.Yaml               as Y

import           Kurokos.GUI.Core
import           Kurokos.GUI.Def
import           Kurokos.GUI.Types
import           Kurokos.GUI.File.Yaml   (YWidget (..), decodeWidgets)
import           Kurokos.GUI.Import
import           Kurokos.GUI.Widget
import           Kurokos.GUI.WidgetTree
import           Kurokos.GUI.Widget.Make

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
convert Single{..} = do
  wt <- genSingle wIdent (V2 wX wY) (V2 wWidth wHeight) =<< genWidget wType
  return $ wt & wtElement._1 %~ setContext
  where
    title = fromMaybe " " wTitle

    genWidget "fill"   = newFill
    genWidget "label"  = newLabel title
    genWidget "button" = newButton title
    genWidget "image"  =
      case wPath of
        Nothing -> E.throwIO $ userError "missing path for image"
        Just path -> newImageView path
    genWidget wtype    = E.throwIO $ userError $ "unkown widget type: " ++ wtype

    setContext ctx =
      ctx & ctxAttrib . visible %~ (flip fromMaybe wVisible)
          & ctxAttrib . clickable %~ (flip fromMaybe wClickable)

convert Container{..} = do
  cnt <- genContainer wIdent wContainerType (V2 wX wY) (V2 wWidth wHeight)
  let cnt' = cnt & wtElement._1 %~ setContext
  ws <- mapM convert wChildren
  let w = mconcat ws
  return $ fromMaybe w $ prependChild cnt' w
  where
    setContext ctx =
      ctx & ctxAttrib . visible %~ (flip fromMaybe wVisible)
          & ctxAttrib . clickable %~ (flip fromMaybe wClickable)
