{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Kurokos.UI.Color
  (
  -- ** Types
    Color
  -- ** WidgetColor
  , WidgetColor (..)
  , wcBack, wcBorder, wcTitle, wcTint
  -- , defaultWidgetColor
  -- ** ContextColor
  , ContextColor (..)
  -- , defaultContextColor
  ) where

import           Control.Lens
import           Data.ByteString         as BS
import           Data.List.Split         (splitOn)
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes)
import           Data.Word               (Word8)
import           Data.Yaml               (FromJSON (..), (.:), (.:?))
import qualified Data.Yaml               as Y

import           SDL.Vect

type Color = V4 Word8

data WidgetColor = WidgetColor
  { _wcBack   :: Color
  , _wcBorder :: Color
  , _wcTitle  :: Color
  , _wcTint   :: Color
  } deriving (Eq, Show, Read)

makeLenses ''WidgetColor

-- defaultWidgetColor :: WidgetColor
-- defaultWidgetColor = WidgetColor
--   { _wcBack   = V4 255 255 255 255
--   , _wcBorder = V4 220 220 220 255
--   , _wcTitle  = V4 54 20 171 255
--   , _wcTint   = V4 220 220 220 255
--   }

instance FromJSON WidgetColor where
  parseJSON (Y.Object v) = WidgetColor
    <$> (parseColor <$> v .: "back")
    <*> (parseColor <$> v .: "border")
    <*> (parseColor <$> v .: "title")
    <*> (parseColor <$> v .: "tint")
    where
      parseColor :: String -> Color
      parseColor str = V4 (read r) (read g) (read b) (read a)
        where
          (r:g:b:a:_) = splitOn " " str
  parseJSON _ = fail "Expected Object for ContextColor"

data ContextColor = ContextColor
  { ctxcolNormal :: WidgetColor
  , ctxcolHover  :: WidgetColor
  , ctxcolClick  :: WidgetColor
  } deriving (Eq, Show, Read)

-- defaultContextColor :: ContextColor
-- defaultContextColor = ContextColor
--   { ctxcolNormal = defaultWidgetColor
--   , ctxcolHover  = defaultWidgetColor & wcBack . _xyz %~ (+ (-10))
--   , ctxcolClick  = defaultWidgetColor
--   }

instance FromJSON ContextColor where
  parseJSON (Y.Object v) = ContextColor
    <$> v .: "normal"
    <*> v .: "hover"
    <*> v .: "click"
  parseJSON _ = fail "Expected Object for ContextColor"
