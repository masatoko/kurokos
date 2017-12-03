module Kurokos.Internal.Archive
  ( archive
  , archiveF
  ) where

import System.IO (withFile, IOMode (..))
import Control.Monad (filterM, foldM_)
import Data.Char (ord)
import Data.Word (Word8)
import Data.Int (Int64)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath.Posix ((</>), takeFileName, splitPath)
import System.PosixCompat.Files (getFileStatus, fileSize)
import System.Posix.Types (FileOffset)
import qualified Data.ByteString as B

import Kurokos.Internal.Util (packSize, (<+>))
import Kurokos.Internal.Encrypt (Seed, encode)

archive :: Seed -> FilePath -> FilePath -> IO ()
archive seed outPath rootDir =
  archiveF seed outPath rootDir (const True)

archiveF :: Seed -> FilePath -> FilePath -> (FilePath -> Bool) -> IO ()
archiveF seed outPath rootDir isValidPath =
  getFiles >>= mapM addFileSize >>= generate outPath
  where
    generate :: FilePath -> [(FilePath, FileOffset)] -> IO ()
    generate outPath fs =
      withFile outPath WriteMode $ \h -> do
        let header = makeHeader fs
            offset = fromIntegral (B.length header) + 4
        B.hPutStr h . encode (seed <+> 0) $ packSize offset
        B.hPutStr h . encode (seed <+> 4) $ header
        foldM_ (work h) offset fs
      where
        work h offset (file, size) = do
          putStrLn $ "archiving '" ++ file ++ "' ..."
          B.hPutStr h =<< (encode (seed <+> offset) <$> B.readFile (rootDir </> file))
          return $ offset + fromIntegral size

    makeHeader :: [(FilePath, FileOffset)] -> B.ByteString
    makeHeader = B.concat . map toInfo
      where
        toInfo (path, size) =
          B.pack $ map charToByte $ show size ++ ":" ++ path ++ "\""

    addFileSize :: FilePath -> IO (FilePath, FileOffset)
    addFileSize path =
      (,) path <$> (fileSize <$> getFileStatus path')
      where
        path' = rootDir </> path

    getFiles :: IO [String]
    getFiles = work ""
      where
        work dir = do
          cs <- map (dir </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents (rootDir </> dir)
          fs <- filterM (doesFileExist . (rootDir </>)) cs
          ds <- filterM (doesDirectoryExist . (rootDir </>)) cs
          let fs' = filter isValidPath fs
          (fs' ++) . concat <$> mapM work ds

charToByte :: Char -> Word8
charToByte = fromIntegral . ord
