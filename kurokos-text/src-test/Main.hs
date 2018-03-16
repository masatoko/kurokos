{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.Map        as M
import           Data.Monoid     ((<>))
import qualified Data.Text       as T
import qualified Data.Text.IO    as T

import           Kurokos.Text

main :: IO ()
main = do
  bytes <- BS.readFile "sample/sample.yaml"
  putStrLn "==="
  work bytes "ja"
  putStrLn "==="
  work bytes "en"
  putStrLn "==="
  where
    work bytes lang = do
      case get of
        Left errmsg -> putStrLn errmsg
        Right t     -> T.putStrLn t
      case testToListWith of
        Left errmsg -> putStrLn errmsg
        Right ts    -> T.putStrLn . T.concat $ ts
      (T.putStrLn . T.concat) =<< testToListWithM
      where
        key = "sample-key"
        get = getText key varmap =<< parseTextSet bytes lang
          where
            varmap = M.fromList [("num", "15")]

        testToListWith = toListWith f key varmap =<< parseTextSet bytes lang
          where
            f text = "[" <> text <> "]"
            varmap = M.fromList [("num", "<15>")]

        testToListWithM =
          case parseTextSet bytes lang of
            Left errmsg -> error errmsg
            Right tset  -> toListWithM return key varmap tset
          where
            varmap = M.fromList [("num", "15")]
