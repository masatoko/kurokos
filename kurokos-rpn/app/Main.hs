module Main where

import Control.Monad (forever)

import Kurokos.RPN

main :: IO ()
main = forever $ do
  eExpr <- parseRPN <$> getLine
  case eExpr of
    Left err   -> putStrLn err
    Right expr -> print $ evaluate expr
