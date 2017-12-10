module Kurokos.Internal.Extract
  ( Archive
  , InternalPath
  , findFile_
  , loadArchive
  , findFile
  , extractFiles
  , files
  , filesWithSize
  ) where

import qualified Control.Exception        as E
import           Control.Monad            (foldM_)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as C
import           Data.Char                (chr, ord)
import           Data.Int                 (Int64)
import           Data.List                (nub)
import           Data.List.Split          (splitOn)
import qualified Data.Map.Strict          as M
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.IO             as T
import           Data.Word                (Word8)
import           System.Directory         (createDirectoryIfMissing)
import           System.FilePath.Posix
import           System.IO.MMap

import           Kurokos.Internal.Encrypt (decode)
import           Kurokos.Internal.Types   (InternalPath, Seed)
import           Kurokos.Internal.Util    (byteToChar, unpackSize, validatePath,
                                           (<+>))

newtype Archive = Archive (M.Map FilePath (B.ByteString, Int64)) deriving Show

-- | Read data directly from archive data path
findFile_ :: Seed -> FilePath -> InternalPath -> IO B.ByteString
findFile_ seed arc target = do
  (headerSize, as) <- headerInfo seed arc
  case break isTarget as of
    (_, []) -> do
      let msg = "Missing '" ++ target ++ "' in '" ++ arc ++ "'"
      E.throwIO $ userError msg
    (ks, (_,size):_) -> do
      let offset = headerSize + fromIntegral (sum (map snd ks))
          range = Just (offset, size)
      bytes <- mmapFileByteString arc range
      return . decode (seed <+> offset) $ bytes
  where
    target' = validatePath target
    isTarget = (`equalFilePath` target') . fst

--

-- | Load 'Archive'
loadArchive :: Seed -> FilePath -> IO Archive
loadArchive seed arc = do
  (offset, infoList) <- headerInfo seed arc
  content <- B.drop (fromIntegral offset) <$> B.readFile arc
  return . Archive . M.fromList $ work offset content infoList
  where
    work _      _     []               = []
    work offset bytes ((path,size):is)
      | B.null bytes = []
      | otherwise    = (path, (bytes', offset)) : work offset' rest is
      where
        offset' = offset + fromIntegral size
        (bytes', rest) = B.splitAt size bytes

-- | Read data from Archive data
findFile :: Seed -> Archive -> InternalPath -> Maybe B.ByteString
findFile seed (Archive amap) path =
  case M.lookup path' amap of
    Just (bytes, offset) -> Just $ decode (seed <+> offset) bytes
    Nothing              -> Nothing
  where
    path' = validatePath path

--

-- | Extract all files from Archive (path)
--
-- - 1st 'FilePath': Archive path
-- - 2nd 'FilePath': Destination directory
extractFiles :: Seed -> FilePath -> FilePath -> IO ()
extractFiles seed arcPath outDir = do
  (headerSize, as) <- headerInfo seed arcPath
  foldM_ work headerSize as
  where
    work offset (path, size) = do
      createDirectoryIfMissing True $ dropFileName outPath
      C.writeFile outPath =<< (decode (seed <+> offset) <$> mmapFileByteString arcPath range)
      return $ offset + fromIntegral size
      where
        range = Just (offset, size)
        outPath = outDir </> path

filesWithSize :: Seed -> FilePath -> IO [(String, Int)]
filesWithSize seed path = snd <$> headerInfo seed path

-- | map fst <$> filesWithSize
files :: Seed -> FilePath -> IO [String]
files seed path = map fst . snd <$> headerInfo seed path

headerInfo :: Seed -> FilePath -> IO (Int64, [(String, Int)])
headerInfo seed path = do
  headerSize <- unpackSize . decode (seed <+> 0) <$> read' 0 4
  headerPart <- decode (seed <+> 4) <$> read' 4 (fromIntegral headerSize - 4)
  info <- toFileInfo . map byteToChar . B.unpack $ headerPart
  return (fromIntegral headerSize, info)
  where
    read' from size = mmapFileByteString path (Just (from, size))

    toFileInfo = mapM toPair . init . splitOn "\""
    toPair part =
      case break (== ':') part of
        (size, ':':path) -> (,) path <$> readIO size
        _                -> E.throwIO $ userError $ "Parse error: " ++ part
