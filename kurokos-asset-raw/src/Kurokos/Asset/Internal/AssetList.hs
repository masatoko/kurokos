{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kurokos.Asset.Internal.AssetList
  ( readAssetList
  , decodeAssetList
  ) where

import qualified Control.Exception      as E
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString        as BS
import           Data.Monoid            ((<>))
import qualified Data.Set               as Set
import qualified Data.Text              as T
import qualified Data.Yaml              as Y
import qualified System.FilePath.Glob   as Glob
import           System.FilePath.Posix  (takeFileName)

import           Kurokos.Asset.Internal.Types

readAssetList :: MonadIO m => FilePath -> m AssetList
readAssetList path = liftIO $
  decodeAssetList =<< BS.readFile path

decodeAssetList :: MonadIO m => BS.ByteString -> m AssetList
decodeAssetList bytes = liftIO $
  case Y.decodeEither' bytes of
    Left e          -> E.throwIO e
    Right assetYaml -> fileToAssetList assetYaml

fileToAssetList :: AssetFile -> IO AssetList
fileToAssetList (AssetFile fs ds) = do
  fs' <- concat <$> mapM toAssetInfo ds
  return $ AssetList $ fs ++ fs'
  where
    toAssetInfo :: PatternsInDir -> IO [AssetInfo]
    toAssetInfo PatternsInDir{..} = do
      incPtn <- compilePattern pidPattern
      includes <- Set.fromList <$> Glob.globDir1 incPtn pidDirectory
      --
      ignPtns <- mapM compilePattern pidIgnores
      ignores <- Set.fromList . concat <$> Glob.globDir ignPtns pidDirectory
      --
      return $ map pathToAssetInfo . Set.toList $ includes `Set.difference` ignores
      where
        pathToAssetInfo path =
          AssetInfo (Just ident) Nothing path'
          where
            path' = map (\c -> if c == '\\' then '/' else c) path
            ident = case pidIdPrefix of
                      Nothing  -> pathid
                      Just pfx -> pfx <> T.cons ':' pathid
            pathid
              | pidIdFname = T.pack $ takeFileName path'
              | otherwise  = T.pack path'

        compilePattern ptnStr =
          case Glob.tryCompileWith Glob.compDefault ptnStr of
            Left err  -> E.throwIO $ userError $ "compiling glob pattern '" ++ ptnStr ++ "' : " ++ err
            Right ptn -> return ptn
