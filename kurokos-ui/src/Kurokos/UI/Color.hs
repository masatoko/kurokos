{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Kurokos.UI.Color
  (
  -- ** Types
    Color
  -- ** WidgetColor
  , WidgetColor (..)
  , wcBack, wcBorder, wcTitle, wcTint
  -- ** ContextColor
  , ContextColor (..)
  ) where

import           Control.Lens
import           Control.Monad.Extra (firstJustM)
import           Data.ByteString     as BS
import           Data.List.Split     (splitOn)
import qualified Data.Map            as M
import           Data.Maybe          (catMaybes)
import qualified Data.Text           as T
import           Data.Word           (Word8)
import           Data.Yaml           (FromJSON (..), (.:), (.:?))
import qualified Data.Yaml           as Y

import           SDL.Vect

type Color = V4 Word8

data WidgetColor = WidgetColor
  { _wcBack   :: Color
  , _wcBorder :: Color
  , _wcTitle  :: Color
  , _wcTint   :: Color
  } deriving (Eq, Show, Read)

makeLenses ''WidgetColor

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

instance FromJSON ContextColor where
  parseJSON (Y.Object v) = ContextColor
    <$> get "normal"
    <*> get "hover"
    <*> get "click"
    where
      get key =
        firstJustM (v .:?) [key, "default"] >>= \case
          Nothing -> fail $ "Missing context color of '" ++ T.unpack key ++ "' nor 'default'"
          Just v  -> return v
  parseJSON _ = fail "Expected Object for ContextColor"
