module Kurokos.Asset
  (
  -- ** Type
    SDLAssetManager
  , Ident
  , AssetList (..)
  -- ** Load
  , decodeAssetList
  , loadAssetManager
  , allocSDL
  -- ** Find Assets
  , lookupBytes
  , lookupFont
  , lookupTexture
  -- ** Encrypt
  , exportAssetManager
  , importAssetManager
  ) where

import qualified Control.Exception               as E
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import qualified Data.ByteString                 as BS
import qualified Data.Yaml                       as Y

import           Kurokos.Internal.Archive.Decode (importAssetManager)
import           Kurokos.Internal.Archive.Encode (exportAssetManager)
import           Kurokos.Internal.AssetManager
import           Kurokos.Internal.Glob
import           Kurokos.Internal.Types

-- testAssets :: MonadIO m => SDL.Renderer -> FilePath -> m ()
-- testAssets r path = liftIO $ do
--   bytes <- BS.readFile path
--   af <- decodeAssetList bytes
--   print af
--   am <- allocSDL r =<< loadAssetManager af
--   print . M.keys . byteMap $ am

decodeAssetList :: MonadIO m => BS.ByteString -> m AssetList
decodeAssetList bytes = liftIO $
  case Y.decodeEither' bytes of
    Left e          -> E.throwIO e
    Right assetYaml -> fileToAssetList assetYaml
