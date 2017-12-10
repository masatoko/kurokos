{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Kurokos.Internal.Glob where

import qualified Control.Exception      as E
import qualified Data.Set as Set
import qualified System.FilePath.Glob   as Glob
import qualified Data.Text as T
import Data.Monoid ((<>))

import           Kurokos.Internal.Types

yamlToAssetList :: AssetYaml -> IO AssetList
yamlToAssetList (AssetYaml fs ds) = do
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
          AssetInfo (Just ident) Nothing path Nothing
          where
            ident = case pidIdPrefix of
              Nothing  -> T.pack path
              Just pfx -> pfx <> T.cons ':' (T.pack path)

        compilePattern ptnStr =
          case Glob.tryCompileWith Glob.compDefault ptnStr of
            Left err  -> E.throwIO $ userError $ "compiling glob pattern '" ++ ptnStr ++ "' : " ++ err
            Right ptn -> return ptn
