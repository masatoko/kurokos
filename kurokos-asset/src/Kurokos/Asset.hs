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

import           Kurokos.Asset.Internal.Archive.Exporter (exportAssetManager)
import           Kurokos.Asset.Internal.Archive.Importer (importAssetManager)
import           Kurokos.Asset.Internal.AssetList        (decodeAssetList)
import           Kurokos.Asset.Internal.AssetManager
import           Kurokos.Asset.Internal.Types

-- test :: MonadIO m => SDL.Renderer -> FilePath -> m ()
-- test r path = liftIO $ do
--   bytes <- BS.readFile path
--   af <- decodeAssetList bytes
--   print af
--   am <- allocSDL r =<< loadAssetManager af
--   print . M.keys . byteMap $ am
