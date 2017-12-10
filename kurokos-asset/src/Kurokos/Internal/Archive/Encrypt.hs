module Kurokos.Internal.Archive.Encrypt
  ( encode
  , decode
  ) where

import           Control.Monad.State
import qualified Crypto.Hash.SHA256            as SHA
import           Data.Bits                     (xor)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import           Data.Int                      (Int64)
import           Data.Word                     (Word8)

import           Kurokos.Internal.Archive.Util (Password, unpackSize)

encode :: Password -> ByteString -> ByteString
encode pw bytes0 =
  B.concat $ evalState (work bytes0) (unpackSize pw)
  where
    work :: ByteString -> State Int64 [ByteString]
    work bytes
      | B.null bytes = return []
      | otherwise    = do
          key <- makeKey <$> get
          modify (+1)
          let (target,rest) = B.splitAt (B.length key) bytes
              coded = B.pack $ B.zipWith xor target key
          (coded:) <$> work rest

    makeKey :: Int64 -> ByteString
    makeKey = SHA.hash . B.pack . intToBytes
      where
        intToBytes :: Int64 -> [Word8]
        intToBytes x
          | x == 0    = []
          | otherwise = fromIntegral (x `mod` 256) : intToBytes (x `div` 256)

decode :: Password -> ByteString -> ByteString
decode = encode
