{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Extra   (whenJust)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict       as M
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           System.FilePath.Posix

import qualified Kurokos.Archive       as ARC

arcPath :: FilePath
arcPath = "_archived/result.arc"

key :: B.ByteString
key = "secret"

main :: IO ()
main = do
  archiveFiles
  extract
  readFromArc

-- Archive files
archiveFiles :: IO ()
archiveFiles =
  ARC.archive key "_sample" arcPath isValidPath
  where
    isValidPath path =
      isHeadValid name && all isValidDir dirs
      where
        name = takeFileName path

        isHeadValid ('@':_) = False
        isHeadValid _       = True

        dirs = splitDirectories . dropFileName $ path
        isValidDir ('_':_) = False
        isValidDir _       = True

-- Extract archive
extract :: IO ()
extract = do
  ARC.extractFiles key arcPath "_extracted"
  putStrLn "\n=== showFiles ==="
  ARC.filesWithSize key arcPath >>= mapM_ print
  ARC.files key arcPath >>= mapM_ print
  putStrLn "\n=== text1.txt ==="
  ARC.findFile_ key arcPath "text1.txt" >>= B.putStrLn
  putStrLn "\n=== child/text2.txt ==="
  ARC.findFile_ key arcPath "child/text2.txt" >>= B.putStrLn
  ARC.findFile_ key arcPath "child/child1/../text2.txt" >>= B.putStrLn
  putStrLn "\n=== filesIn ==="
  fs <- ARC.files key arcPath
  print $ ARC.filesIn fs "child"
  putStrLn "\n=== dirsIn ==="
  print $ ARC.dirsIn fs "child"
  print $ ARC.dirsIn fs ""
  putStrLn "\n=== filesIn ==="
  print $ ARC.filesIn fs "child"
  print $ ARC.filesIn fs ""

-- Read an archive file as 'Archive' data
readFromArc :: IO ()
readFromArc = do
  putStrLn "\n=== Read archive ==="
  arc <- ARC.loadArchive key arcPath -- read an archive file and make 'Archive' data
  whenJust (ARC.findFile key arc "child/child1/txt_child1.txt") print -- read data from the Archive data
