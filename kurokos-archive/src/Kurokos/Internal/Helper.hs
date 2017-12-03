module Kurokos.Internal.Helper where

import           Data.List              (isPrefixOf, nub)
import           Data.Maybe             (mapMaybe)
import           Safe                   (headMay)
import           System.FilePath.Posix

import           Kurokos.Internal.Types (InternalPath)
import           Kurokos.Internal.Util  (validatePath)

dirsIn :: [FilePath] -> InternalPath -> [FilePath]
dirsIn fs dir =
  work2 . work1 $ fs
  where
    work1 = nub . mapMaybe childDir . filter (dir' `isPrefixOf`)
    work2 = map (dir' ++) . filter (/= ".")

    dir'
      | null dir  = dir
      | otherwise = addTrailingPathSeparator . validatePath $ dir
    n = length $ splitDirectories dir'
    childDir = headMay . drop n . splitDirectories . dropFileName

filesIn :: [FilePath] -> InternalPath -> [FilePath]
filesIn fs dir =
  filter isChild fs
  where
    isChild path
      | null dir  = dropFileName path == "./"
      | otherwise = dropFileName path == dir'

    dir'
      | null dir  = dir
      | otherwise = addTrailingPathSeparator . validatePath $ dir
