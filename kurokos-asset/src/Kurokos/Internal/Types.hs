{-# LANGUAGE OverloadedStrings #-}
module Kurokos.Internal.Types where

import qualified Data.ByteString      as BS
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Data.Yaml            (FromJSON (..), (.:), (.:?))
import qualified Data.Yaml            as Y
import Data.Maybe (fromMaybe)

import qualified SDL
import qualified SDL.Font             as Font

type Ident = T.Text

data AssetInfo = AssetInfo
  { aiIdent     :: Maybe Ident
  , aiDirectory :: Maybe FilePath
  , aiFileName  :: String
  -- , aiSize      :: Maybe Int -- Font point size
  } deriving Show

instance FromJSON AssetInfo where
  parseJSON (Y.Object v) = AssetInfo
    <$> v .:? "id"
    <*> v .:? "dir"
    <*> v .:  "file"
    -- <*> v .:? "size"
  parseJSON _ = fail "Expected Object for AssetInfo"

data PatternsInDir = PatternsInDir
  { pidIdPrefix  :: Maybe Ident
  , pidIdFname   :: Bool
  , pidDirectory :: FilePath
  , pidPattern   :: String
  , pidIgnores   :: [String]
  }

instance FromJSON PatternsInDir where
  parseJSON (Y.Object v) = PatternsInDir
    <$> v .:? "id"
    <*> (fromMaybe True <$> v .:? "id-fname")
    <*> v .: "dir"
    <*> v .: "pattern"
    <*> (fromMaybe [] <$> v .:? "ignores")
  parseJSON _            = fail "Expected Object for PatternsInDir"

data AssetFile = AssetFile [AssetInfo] [PatternsInDir]

instance FromJSON AssetFile where
  parseJSON (Y.Object v) = AssetFile
    <$> (fromMaybe [] <$> v .:? "files")
    <*> (fromMaybe [] <$> v .:? "patterns")
  parseJSON _            = fail "Expected Object for AssetFile"

newtype AssetList =
  AssetList { unAssetList :: [AssetInfo] }
  deriving Show

instance Monoid AssetList where
  mempty = AssetList []
  mappend (AssetList xs) (AssetList ys) = AssetList $ xs ++ ys

newtype AssetManager = AssetManager
  { unAstMng :: M.Map Ident (AssetInfo, BS.ByteString)
  }

data SDLAssetManager = SDLAssetManager
  { byteMap    :: M.Map Ident BS.ByteString
  , fontMap    :: M.Map Ident BS.ByteString
  , textureMap :: M.Map Ident SDL.Texture
  }
