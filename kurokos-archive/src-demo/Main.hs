{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.FilePath.Posix
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as B

import Kurokos.Archive

main :: IO ()
main = do
  B.putStrLn . decode key . encode key $ "KONNICHIWA!"
  -- Archive files
  archiveF key arcPath "_target" isValidPath
  -- Extract files
  extractFiles key arcPath "_extracted"
  putStrLn "\n=== showFiles ==="
  showFiles key arcPath >>= putStrLn
  putStrLn "\n=== text1.txt ==="
  readArchiveText key arcPath "text1.txt" >>= T.putStrLn
  putStrLn "\n=== child/text2.txt ==="
  readArchiveStr key arcPath "child/text2.txt" >>= putStrLn
  readArchiveStr key arcPath "child/child1/../text2.txt" >>= putStrLn
  putStrLn "\n=== directoryFiles ==="
  directoryFiles key arcPath "child" >>= print
  putStrLn "\n=== directoryDirs ==="
  directoryDirs key arcPath "child" >>= print
  directoryDirs key arcPath "" >>= print
  putStrLn "\n=== directoryFiles ==="
  directoryFiles key arcPath "child" >>= print
  directoryFiles key arcPath "" >>= print

  -- Read data from the archive file
  putStrLn "\n=== Read from archive ==="
  arc <- readArchive key arcPath
  bs <- getFileBS key "child/child1/txt_child1.txt" arc
  print bs
  where
    arcPath = "_archived/result.arc"
    key = "secret"

    isValidPath path =
      isHeadValid name && all isValidDir dirs
      where
        name = takeFileName path

        isHeadValid ('@':_) = False
        isHeadValid _       = True

        dirs = splitDirectories . dropFileName $ path
        isValidDir ('_':_) = False
        isValidDir _       = True
