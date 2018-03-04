module Kurokos.UI.Widget.Module where

import           Data.Text          (Text)

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
getBool _ = Nothing

-- | Get current value of Int Slider
getInt :: Widget -> Maybe Int
getInt (Slider _ _ _ _ (ValueI v _ _)) = Just v
getInt _ = Nothing

-- | Get current value of Float Slider
getFloat :: Widget -> Maybe Float
getFloat (Slider _ _ _ _ (ValueF v _ _)) = Just v
getFloat _ = Nothing

-- | Get current value of Double Slider
getDouble :: Widget -> Maybe Double
getDouble (Slider _ _ _ _ (ValueD v _ _)) = Just v
getDouble _ = Nothing
