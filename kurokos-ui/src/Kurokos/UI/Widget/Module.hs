module Kurokos.UI.Widget.Module where

import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.Zipper  as TZ
import           Safe              (atMay, readMay)

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
getText (TextField z _) = Just $ TZ.currentLine z
getText _               = Nothing

-- | Get key of Picker widget
getKey :: Widget -> Maybe String
getKey (Picker ts idx _) = fst <$> ts `atMay` idx
getKey _                 = Nothing

-- * Control

widgetLeft :: Widget -> Widget
widgetLeft (TextField z mRsc) = TextField (TZ.moveLeft z) mRsc
widgetLeft w = w

widgetRight :: Widget -> Widget
widgetRight (TextField z mRsc) = TextField (TZ.moveRight z) mRsc
widgetRight w = w

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
