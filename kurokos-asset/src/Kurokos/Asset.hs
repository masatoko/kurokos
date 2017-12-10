module Kurokos.Asset
  ( testAssets
  -- ** Type
  , AssetManager
  , Ident
  -- ** Load
  , decodeAssetFile
  , loadAssetManager
  -- ** Find Assets
  , lookupBytes
  , lookupFont
  , lookupTexture
  ) where

import qualified Control.Exception          as E
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Data.ByteString            as BS
import qualified Data.Map                   as M
import qualified Data.Yaml                  as Y

import qualified SDL

import           Kurokos.Asset.AssetManager
import           Kurokos.Asset.Types

testAssets :: MonadIO m => SDL.Renderer -> FilePath -> m ()
testAssets r path = liftIO $ do
  bytes <- BS.readFile path
  af <- decodeAssetFile bytes
  print af
  am <- loadAssetManager r af
  print . M.keys . byteMap $ am

decodeAssetFile :: MonadIO m => BS.ByteString -> m AssetFile
decodeAssetFile bytes = liftIO $
  case Y.decodeEither' bytes of
    Left e   -> E.throwIO e
    Right af -> return af