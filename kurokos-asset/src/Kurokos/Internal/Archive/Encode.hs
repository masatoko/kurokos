module Kurokos.Internal.Archive.Encode
  ( exportAssetManager
  ) where

import           Control.Monad                    (foldM_)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as C8
import qualified Data.Map                         as M
import qualified Data.Text                        as T
import qualified System.IO                        as IO

import           Kurokos.Internal.Archive.Encrypt (encode)
import           Kurokos.Internal.Archive.Util    (Password, packSize, (<+>))
import           Kurokos.Internal.Types

exportAssetManager :: Password -> FilePath -> AssetManager -> IO ()
exportAssetManager pwd dest (AssetManager amap) =
  generate $ M.toList amap
  where
    generate :: [(Ident, (FilePath, BS.ByteString))] -> IO ()
    generate fs =
      IO.withFile dest IO.WriteMode $ \h -> do
        let offset = fromIntegral (BS.length header) + 4
        BS.hPutStr h . encode (pwd <+> 0) $ packSize offset
        BS.hPutStr h . encode (pwd <+> 4) $ header
        foldM_ (work h) offset fs
      where
        work h offset (ident, (file, bytes)) = do
          putStrLn $ "Archiving '" ++ T.unpack ident ++ ": " ++ file ++ "' ..."
          BS.hPutStr h coded
          return offset'
          where
            size = fromIntegral $ BS.length bytes
            offset' = offset + size
            coded = encode (pwd <+> offset) bytes

        header :: BS.ByteString
        header = BS.concat . map toInfo $ fs
          where
            toInfo (ident, (path, bytes)) = C8.pack info
              where
                info = show size ++ ":" ++ T.unpack ident ++ ":" ++ path ++ ";"
                size = BS.length bytes
