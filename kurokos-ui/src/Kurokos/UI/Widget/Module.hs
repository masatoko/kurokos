module Kurokos.UI.Widget.Module where

import           Control.Lens
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.Zipper  as TZ
import           Safe              (atMay, readMay)

import           Kurokos.UI.Core
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget

-- | Get current state of Switch
getBool :: Widget -> Maybe Bool
getBool (Switch _ bool) = Just bool
getBool _               = Nothing

-- | Get current value of Int Slider
getInt :: Widget -> Maybe Int
getInt (Slider _ _ (ValueI v _ _)) = Just v
getInt (TextField z _)             = readMay . T.unpack . TZ.currentLine $ z
getInt _                           = Nothing

-- | Get current value of Float Slider
getFloat :: Widget -> Maybe Float
getFloat (Slider _ _ (ValueF v _ _)) = Just v
getFloat (TextField z _)             = readMay . T.unpack . TZ.currentLine $ z
getFloat _                           = Nothing

-- | Get current value of Double Slider
getDouble :: Widget -> Maybe Double
getDouble (Slider _ _ (ValueD v _ _)) = Just v
getDouble (TextField z _)             = readMay . T.unpack . TZ.currentLine $ z
getDouble _                           = Nothing

getText :: Widget -> Maybe T.Text
getText (TextField z _) = Just $
  case unsnoc line of
    Nothing         -> line
    Just (line', c) -> if c == ' ' then line' else line
  where
    line = TZ.currentLine z
    unsnoc :: T.Text -> Maybe (T.Text, Char)
    unsnoc ts
      | T.null ts = Nothing
      | otherwise = Just (T.init ts, T.last ts)
getText _               = Nothing

setText :: T.Text -> CtxWidget -> CtxWidget
setText text (ctx, Label _) = (ctx', Label text)
  where
    ctx' = ctx&ctxNeedsResize .~ True
              &ctxNeedsRender .~ True
setText text (ctx, TextField _ r) = (ctx', TextField z r)
  where
    ctx' = ctx&ctxNeedsResize .~ True
              &ctxNeedsRender .~ True
    z = TZ.textZipper [text] Nothing
setText _ cw = cw

setValueI :: Integral a => a -> CtxWidget -> CtxWidget
setValueI v cw@(ctx, Slider t rsc value) =
  case mv of
    Nothing     -> cw
    Just value' -> (ctx', Slider t rsc value')
  where
    mv = case value of
      ValueI _ amin amax -> Just $ ValueI (fromIntegral v) amin amax
      ValueF _ amin amax -> Just $ ValueF (fromIntegral v) amin amax
      ValueD _ amin amax -> Just $ ValueD (fromIntegral v) amin amax
      _                  -> Nothing
    ctx' = ctx&ctxNeedsResize .~ True
              &ctxNeedsRender .~ True
setValueI _ cw = cw

-- | Get key of Picker widget
getKey :: Widget -> Maybe String
getKey (Picker ts idx _) = fst <$> ts `atMay` idx
getKey _                 = Nothing

-- * Control

widgetLeft :: Widget -> Widget
widgetLeft (TextField z mRsc) = TextField (TZ.moveLeft z) mRsc
widgetLeft w                  = w

widgetRight :: Widget -> Widget
widgetRight (TextField z mRsc) = TextField (TZ.moveRight z) mRsc
widgetRight w                  = w

widgetInputText :: T.Text -> Widget -> Widget
widgetInputText text (TextField z mRsc) =
  TextField (TZ.insertMany text z) mRsc
widgetInputText _ w = w

widgetDeleteChar :: Widget -> Widget
widgetDeleteChar (TextField z mRsc) =
  TextField (TZ.deleteChar z) mRsc
widgetDeleteChar w = w

widgetBackspace :: Widget -> Widget
widgetBackspace (TextField z mRsc) =
  TextField (TZ.deletePrevChar z) mRsc
widgetBackspace w = w
