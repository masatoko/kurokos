module Kurokos.Internal.Archive
  ( archiveAll
  , archive
  ) where

import           Control.Monad            (filterM, foldM_)
import qualified Data.ByteString          as B
import           System.Directory         (doesDirectoryExist, doesFileExist,
                                           getDirectoryContents)
import           System.FilePath.Posix    ((</>))
import           System.IO                (IOMode (..), withFile)
import           System.Posix.Types       (FileOffset)
import           System.PosixCompat.Files (fileSize, getFileStatus)

import           Kurokos.Internal.Encrypt (encode)
import           Kurokos.Internal.Types   (Seed)
import           Kurokos.Internal.Util    (charToByte, packSize, (<+>))

-- | Archive files.
--
-- - 1st 'FilePath': Root directory for files you want to archive
-- - 2nd 'FilePath': Destination path
archiveAll :: Seed -> FilePath -> FilePath -> IO ()
archiveAll seed rootDir outPath =
  archive seed rootDir outPath (const True)

-- | Archive files with path validator.
--
-- - 1st 'FilePath': Root directory for files you want to archive
-- - 2nd 'FilePath': Destination path
archive :: Seed -> FilePath -> FilePath -> (FilePath -> Bool) -> IO ()
archive seed rootDir outPath isValidPath =
  getFiles >>= mapM addFileSize >>= generate
  where
    generate :: [(FilePath, FileOffset)] -> IO ()
    generate fs =
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
