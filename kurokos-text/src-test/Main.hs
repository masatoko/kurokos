{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.Map        as M
import qualified Data.Text.IO    as T

import           Kurokos.Text

main :: IO ()
main = do
  bytes <- BS.readFile "sample/sample.yaml"
  work bytes "ja"
  work bytes "en"
  where
    varmap = M.fromList [("num", "15")]
    work bytes lang =
      case get of
        Left errmsg -> putStrLn errmsg
        Right t     -> T.putStrLn t
      where
        get = do
          tset <- parseTextSet bytes lang
          getText "sample-key" varmap tset
