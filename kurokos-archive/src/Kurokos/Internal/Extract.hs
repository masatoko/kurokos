module Kurokos.Internal.Extract
  ( Archive
  , readArchiveBS
  , readArchiveText
  , readArchiveStr
  , readArchive
  , getFileBS
  , getFileText
  , getFileStr
  , extractFiles
  --
  , directoryDirs
  , directoryFiles
  , allFiles
  , showFiles
  ) where

import Control.Monad (foldM_)
import qualified Control.Exception as E
import Data.Maybe (mapMaybe)
import Data.Char (chr, ord)
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List (isPrefixOf, isInfixOf, nub, intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import System.Directory (createDirectoryIfMissing)
import System.Posix.Types (FileOffset)
import System.FilePath.Posix
import System.IO.MMap
import Safe (readMay, headMay)

import Kurokos.Internal.Util (unpackSize, (<+>))
import Kurokos.Internal.Encrypt (Seed, decode)

newtype Archive = Archive (M.Map FilePath (B.ByteString, Int64)) deriving Show

type InternalPath = String

readArchiveStr :: Seed -> FilePath -> InternalPath -> IO String
readArchiveStr seed arc target = T.unpack <$> readArchiveText seed arc target

readArchiveText :: Seed -> FilePath -> InternalPath -> IO T.Text
readArchiveText seed arc target = T.decodeUtf8 <$> readArchiveBS seed arc target

readArchiveBS :: Seed -> FilePath -> InternalPath -> IO B.ByteString
readArchiveBS seed arc target = do
  (headerSize, as) <- headerInfo seed arc
  case break isTarget as of
    (_, []) -> do
      let msg = "Missing '" ++ target ++ "' in '" ++ arc ++ "'"
      E.throwIO $ userError msg
    (ks, ((_,size):_)) -> do
      let offset = headerSize + fromIntegral (sum (map snd ks))
          range = Just (offset, size)
      bytes <- mmapFileByteString arc range
      return . decode (seed <+> offset) $ bytes
  where
    target' = validatePath target
    isTarget = (`equalFilePath` target') . fst

--

readArchive :: Seed -> FilePath -> IO Archive
readArchive seed arc = do
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

getFileStr :: Seed -> InternalPath -> Archive -> IO String
getFileStr seed path arc = T.unpack <$> getFileText seed path arc

getFileText :: Seed -> InternalPath -> Archive -> IO T.Text
getFileText seed path arc = T.decodeUtf8 <$> getFileBS seed path arc

getFileBS :: Seed -> InternalPath -> Archive -> IO B.ByteString
getFileBS seed path (Archive amap) =
  case M.lookup path' amap of
    Just (bytes, offset) -> return $ decode (seed <+> offset) bytes
    Nothing              -> E.throwIO $ userError $ "Missing '" ++ path ++ "' in archive data"
  where
    path' = validatePath path

--

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

directoryDirs :: Seed -> FilePath -> FilePath -> IO [FilePath]
directoryDirs seed arc dir = do
  fs <- allFiles seed arc
  let as = nub . mapMaybe childDir $ filter (dir' `isPrefixOf`) fs
  return $ map (dir' ++) $ filter (/= ".") as
  where
    dir' = if null dir
             then dir
             else addTrailingPathSeparator . validatePath $ dir
    n = length $ splitDirectories dir'
    childDir = headMay . drop n . splitDirectories . dropFileName

directoryFiles :: Seed -> FilePath -> FilePath -> IO [FilePath]
directoryFiles seed arc dir =
  filter isChild <$> allFiles seed arc
  where
    isChild path
      | null dir  = dropFileName path == "./"
      | otherwise = dropFileName path == dir'

    dir' = if null dir
             then dir
             else addTrailingPathSeparator . validatePath $ dir

allFiles :: Seed -> FilePath -> IO [String]
allFiles seed path = map fst . snd <$> headerInfo seed path

showFiles :: Seed -> FilePath -> IO String
showFiles seed path = do
  (_, cs) <- headerInfo seed path
  return $ unlines $ map work cs
  where
    work (path, size) = path ++ ": " ++ show size

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

validatePath :: FilePath -> FilePath
validatePath path
  | "../" `isInfixOf` path = validatePath $
      if null a
        then intercalate "../" as
        else initDirs a </> intercalate "../" as
  | otherwise              = path
  where
    a:as = splitOn "../" path
    initDirs = joinPath . init . splitDirectories

charToByte :: Char -> Word8
charToByte = fromIntegral . ord

byteToChar :: Word8 -> Char
byteToChar = chr . fromIntegral
