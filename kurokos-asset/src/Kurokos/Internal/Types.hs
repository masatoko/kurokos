{-# LANGUAGE OverloadedStrings #-}
module Kurokos.Internal.Types where

import qualified Data.ByteString as BS
import qualified Data.Map        as M
import qualified Data.Text       as T
import           Data.Yaml       (FromJSON (..), (.:), (.:?))
import qualified Data.Yaml       as Y

import qualified SDL
import qualified SDL.Font        as Font

type Ident = T.Text

data AssetInfo = AssetInfo
  { aiIdent     :: Maybe Ident
  , aiDirectory :: Maybe FilePath
  , aiFileName  :: String
  , aiSize      :: Maybe Int
  } deriving Show

instance FromJSON AssetInfo where
  parseJSON (Y.Object v) = AssetInfo
    <$> v .:? "id"
    <*> v .:? "dir"
    <*> v .:  "file"
    <*> v .:? "size"
  parseJSON _ = fail "Expected Object for AssetInfo"

newtype AssetFile = AssetFile [AssetInfo] deriving Show

instance FromJSON AssetFile where
  parseJSON (Y.Object v) = AssetFile
    <$> v .: "assets"
  parseJSON _            = fail "Expected Object for AssetFile"

newtype AssetManager = AssetManager
  { unAstMng :: M.Map Ident (AssetInfo, BS.ByteString)
  }

data SDLAssetManager = SDLAssetManager
  { byteMap    :: M.Map Ident BS.ByteString
  , fontMap    :: M.Map Ident Font.Font
  , textureMap :: M.Map Ident SDL.Texture
  }
