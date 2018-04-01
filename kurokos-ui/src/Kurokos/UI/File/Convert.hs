{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kurokos.UI.File.Convert
  ( readWidgetTree
  , parseWidgetTree
  ) where

import Debug.Trace (trace)

import qualified Control.Exception       as E
import           Control.Lens
import qualified Data.ByteString         as BS
import qualified Data.Yaml               as Y
import           Safe                    (readMay)

import           Kurokos.UI.Core
import           Kurokos.UI.Def
import           Kurokos.UI.File.Yaml    (Title (..), YValue (..), YWidget (..),
                                          YWidgetAttrib (..), decodeWidgets)
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
  wt <- mkSingle conf =<< generate
  return $ wt & wtElement._1 %~ setContext
  where
    conf = WidgetConfig wName wColor wStyle Nothing wX wY wWidth wHeight
    Title titleText titleSize titleAssetIdent = fromMaybe (error "Missing title") wTitle
    generate
      | wType == N.wnameFill      = newFill
      | wType == N.wnameLabel     = newLabel titleAssetIdent titleSize titleText
      | wType == N.wnameButton    = newButton titleAssetIdent titleSize titleText
      | wType == N.wnameSwitch    = newSwitch titleAssetIdent titleSize titleText
      | wType == N.wnameSlider    = newSlider titleAssetIdent titleSize titleText value
      | wType == N.wnameImageView = newImageView =<< getAssetId
      | wType == N.wnameTextField = newTextField titleAssetIdent titleSize titleText
      | otherwise                 = liftIO $ E.throwIO $ userError $ "unkown widget type: " ++ wType

    getAssetId = case wAsset of
                  Nothing      -> liftIO $ E.throwIO $ userError $ "missing asset: " ++ show s
                  Just assetid -> return assetid

    setContext ctx = case wAttrib of
      Nothing -> ctx
      Just ywa@YWidgetAttrib{..} -> trace (show ywa) $
        ctx&ctxAttrib . hoverable %~ flip fromMaybe ywaHoverable
           &ctxAttrib . clickable %~ flip fromMaybe ywaClickable
           &ctxAttrib . draggable %~ flip fromMaybe ywaDraggable
           &ctxAttrib . droppable %~ flip fromMaybe ywaDroppable
           &ctxAttrib . visible   %~ flip fromMaybe ywaVisible

    value = maybe (error msg) make wValue
      where
        msg = "Missing value for slider " ++ maybe "." (\name -> "named " ++ name ++ ".") wName
        make YValue{..} = toValue yvType
          where
            toValue "int"    = ValueI (maybe 0 round yvDef) (round yvMin) (round yvMax)
            toValue "float"  = ValueF (maybe 0 realToFrac yvDef) (realToFrac yvMin) (realToFrac yvMax)
            toValue "double" = ValueD (fromMaybe 0 yvDef) yvMin yvMax
            toValue tpstr    = error $ "Undefined type: " ++ tpstr

convert Container{..} = do
  cnt <- mkContainer conf wContainerType
  let cnt' = cnt & wtElement._1 %~ setContext
  ws <- mapM convert wChildren
  return $ appendChild cnt' (mconcat ws)
  where
    conf = WidgetConfig wName wColor wStyle Nothing wX wY wWidth wHeight
    setContext ctx = case wAttrib of
      Nothing -> ctx
      Just ywa@YWidgetAttrib{..} -> trace (show ywa) $
        ctx&ctxAttrib . hoverable %~ flip fromMaybe ywaHoverable
           &ctxAttrib . clickable %~ flip fromMaybe ywaClickable
           &ctxAttrib . draggable %~ flip fromMaybe ywaDraggable
           &ctxAttrib . droppable %~ flip fromMaybe ywaDroppable
           &ctxAttrib . visible   %~ flip fromMaybe ywaVisible
