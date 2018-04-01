module Kurokos.UI.Widget.Module where

import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.Zipper  as TZ

import           Kurokos.UI.Import
import           Kurokos.UI.Widget

-- TODO: Implement
-- setTitle :: Text -> Widget -> Widget
-- setTitle _     w@Transparent   = w
-- setTitle _     w@Fill          = w
-- setTitle title (Label _ font)  = Label title font
-- setTitle _     w@ImageView{}   = w
-- setTitle title (Button _ font) = Button title font
-- setTitle _     w@UserWidget{}  = w

-- | Get current state of Switch
getBool :: Widget -> Maybe Bool
getBool (Switch _ _ _ bool) = Just bool
getBool _                   = Nothing

-- | Get current value of Int Slider
getInt :: Widget -> Maybe Int
getInt (Slider _ _ _ _ (ValueI v _ _)) = Just v
getInt _                               = Nothing

-- | Get current value of Float Slider
getFloat :: Widget -> Maybe Float
getFloat (Slider _ _ _ _ (ValueF v _ _)) = Just v
getFloat _                               = Nothing

-- | Get current value of Double Slider
getDouble :: Widget -> Maybe Double
getDouble (Slider _ _ _ _ (ValueD v _ _)) = Just v
getDouble _                               = Nothing


-- * Control

widgetLeft :: Widget -> Widget
widgetLeft (TextField font size z mRsc) = TextField font size (TZ.moveLeft z) mRsc
widgetLeft w = w

widgetRight :: Widget -> Widget
widgetRight (TextField font size z mRsc) = TextField font size (TZ.moveRight z) mRsc
widgetRight w = w

widgetInputText :: T.Text -> Widget -> Widget
widgetInputText text (TextField font size z mRsc) =
  TextField font size (TZ.insertMany text z) mRsc
widgetInputText _ w = w

widgetDeleteChar :: Widget -> Widget
widgetDeleteChar (TextField font size z mRsc) =
  TextField font size (TZ.deleteChar z) mRsc
widgetDeleteChar w = w

widgetBackspace :: Widget -> Widget
widgetBackspace (TextField font size z mRsc) =
  TextField font size (TZ.deletePrevChar z) mRsc
widgetBackspace w = w
