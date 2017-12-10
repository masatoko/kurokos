module Kurokos.Asset
  (
  -- ** Type
    SDLAssetManager
  , Ident
  -- ** Load
  , decodeAssetFile
  , loadAssetManager
  , allocSDL
  -- ** Find Assets
  , lookupBytes
  , lookupFont
  , lookupTexture
  ) where

import qualified Control.Exception             as E
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import qualified Data.ByteString               as BS
import qualified Data.Yaml                     as Y

import           Kurokos.Internal.AssetManager
import           Kurokos.Internal.Types

-- testAssets :: MonadIO m => SDL.Renderer -> FilePath -> m ()
-- testAssets r path = liftIO $ do
--   bytes <- BS.readFile path
--   af <- decodeAssetFile bytes
--   print af
--   am <- allocSDL r =<< loadAssetManager af
--   print . M.keys . byteMap $ am

decodeAssetFile :: MonadIO m => BS.ByteString -> m AssetFile
decodeAssetFile bytes = liftIO $
  case Y.decodeEither' bytes of
    Left e   -> E.throwIO e
    Right af -> return af
