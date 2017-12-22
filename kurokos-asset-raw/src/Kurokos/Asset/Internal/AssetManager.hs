{-# LANGUAGE RecordWildCards #-}
module Kurokos.Asset.Internal.AssetManager where

import qualified Control.Exception as E
import qualified System.IO as IO
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString        as BS
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import           System.FilePath.Posix

import           Kurokos.Asset.Internal.Types

loadAssetManager :: MonadIO m => AssetList -> m RawAssetManager
loadAssetManager (AssetList as) =
  RawAssetManager . M.fromList <$> liftIO (mapM work as) -- TODO: Shold check whether keys are duplicated or not.
  where
    work AssetInfo{..} =
      IO.withFile path IO.ReadMode $ \h -> do
        bytes <- BS.hGetContents h
        E.evaluate bytes
        return (ident', (path, bytes))
      where
        path =
          case aiDirectory of
            Nothing  -> aiFileName
            Just dir -> dir </> aiFileName
        ident' = fromMaybe (T.pack aiFileName) aiIdent
