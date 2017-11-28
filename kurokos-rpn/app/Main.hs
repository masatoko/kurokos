module Main where

import Control.Monad (forever)

import Kurokos.RPN

main :: IO ()
main = forever $ do
  eExpr <- parse <$> getLine
  case eExpr of
    Left err   -> putStrLn err
    Right expr -> print $ eval expr
