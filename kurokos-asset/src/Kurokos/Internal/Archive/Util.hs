module Kurokos.Internal.Archive.Util
  ( Password
  , packSize
  , unpackSize
  , (<+>)
  ) where

import qualified Data.ByteString       as BS
import           Data.Int              (Int64)
import           Data.Word             (Word8)

type Password = BS.ByteString

packSize :: Int64 -> BS.ByteString
packSize n =
  BS.pack . take 4 $ work n ++ repeat 0
  where
    work :: Int64 -> [Word8]
    work x
      | x == 0    = []
      | otherwise = fromIntegral (x `mod` 256) : work (x `div` 256)

unpackSize :: BS.ByteString -> Int64
unpackSize = work 1 . take 4 . BS.unpack
  where
    work :: Int64 -> [Word8] -> Int64
    work _    []     = 0
    work base (w:ws) =
      base * fromIntegral w + work (base * 256) ws

(<+>) :: BS.ByteString -> Int64 -> BS.ByteString
(<+>) bytes x = packSize $ unpackSize bytes + x
