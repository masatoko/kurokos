module Kurokos.Internal.Util
  ( packSize
  , unpackSize
  , (<+>)
  ) where

import Data.Int (Int64)
import Data.Word (Word8)
import qualified Data.ByteString as B

packSize :: Int64 -> B.ByteString
packSize n =
  B.pack . take 4 $ work n ++ repeat 0
  where
    work :: Int64 -> [Word8]
    work x
      | x == 0    = []
      | otherwise = fromIntegral (x `mod` 256) : work (x `div` 256)

unpackSize :: B.ByteString -> Int64
unpackSize = work 1 . take 4 . B.unpack
  where
    work :: Int64 -> [Word8] -> Int64
    work _    []     = 0
    work base (w:ws) =
      base * fromIntegral w + work (base * 256) ws

(<+>) :: B.ByteString -> Int64 -> B.ByteString
(<+>) bytes x = packSize $ unpackSize bytes + x
