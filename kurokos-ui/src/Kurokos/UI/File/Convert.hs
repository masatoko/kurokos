{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kurokos.UI.File.Convert
  ( readWidgetTree
  , parseWidgetTree
  ) where

import qualified Control.Exception       as E
import           Control.Lens
import qualified Data.ByteString         as BS
import qualified Data.Yaml               as Y

import           Kurokos.UI.Core
import           Kurokos.UI.Def
import           Kurokos.UI.File.Yaml    (Title (..), YWidget (..),
                                          decodeWidgets)
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget
import           Kurokos.UI.Widget.Make
import qualified Kurokos.UI.Widget.Names as N
import           Kurokos.UI.WidgetTree

readWidgetTree :: (RenderEnv m, MonadIO m) => FilePath -> GuiT m GuiWidgetTree
readWidgetTree path =
  parseWidgetTree =<< liftIO (BS.readFile path)

parseWidgetTree :: (RenderEnv m, MonadIO m)
  => BS.ByteString -> GuiT m GuiWidgetTree
parseWidgetTree bs =
  case decodeWidgets bs of
    Left e   -> liftIO $ E.throwIO e
    Right ys -> work ys
  where
    work ys = do
      ws <- mapM convert ys
      return $ mconcat ws

convert :: (RenderEnv m, MonadIO m)
  => YWidget -> GuiT m GuiWidgetTree
convert s@Single{..} = do
  wt <- mkSingle wName wColor (V2 wX wY) (V2 wWidth wHeight) =<< generate
  return $ wt & wtElement._1 %~ setContext
  where
    Title titleText titleSize titleAssetIdent = fromMaybe (error "Missing title") wTitle
    generate
      | wType == N.wnameFill      = newFill
      | wType == N.wnameLabel     = newLabel titleAssetIdent titleText titleSize
      | wType == N.wnameButton    = newButton titleAssetIdent titleText titleSize
      | wType == N.wnameImageView = newImageView =<< getAssetId
      | otherwise                 = liftIO $ E.throwIO $ userError $ "unkown widget type: " ++ wType

    getAssetId = case wAsset of
                  Nothing      -> liftIO $ E.throwIO $ userError $ "missing asset: " ++ show s
                  Just assetid -> return assetid

    setContext ctx =
      ctx & ctxAttrib . visible %~ flip fromMaybe wVisible
          & ctxAttrib . clickable %~ flip fromMaybe wClickable
          & ctxAttrib . hoverable %~ flip fromMaybe wHoverable

convert Container{..} = do
  cnt <- mkContainer wName wContainerType wColor (V2 wX wY) (V2 wWidth wHeight)
  let cnt' = cnt & wtElement._1 %~ setContext
  ws <- mapM convert wChildren
  let w = mconcat ws
  return $ fromMaybe w $ prependChild cnt' w
  where
    setContext ctx =
      ctx & ctxAttrib . visible %~ flip fromMaybe wVisible
          & ctxAttrib . clickable %~ flip fromMaybe wClickable
          & ctxAttrib . hoverable %~ flip fromMaybe wHoverable
