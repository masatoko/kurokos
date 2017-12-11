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

import           Kurokos.Internal.Archive.Exporter (exportAssetManager)
import           Kurokos.Internal.Archive.Importer (importAssetManager)
import           Kurokos.Internal.AssetList        (decodeAssetList)
import           Kurokos.Internal.AssetManager
import           Kurokos.Internal.Types

-- test :: MonadIO m => SDL.Renderer -> FilePath -> m ()
-- test r path = liftIO $ do
--   bytes <- BS.readFile path
--   af <- decodeAssetList bytes
--   print af
--   am <- allocSDL r =<< loadAssetManager af
--   print . M.keys . byteMap $ am
