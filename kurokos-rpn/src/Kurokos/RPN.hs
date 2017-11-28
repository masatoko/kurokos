module Kurokos.RPN
  ( parse
  , eval
  ) where

import qualified Control.Exception as E
import           Data.Fixed        (mod')
import           Data.Foldable     (foldlM)
import           Safe              (headMay, readMay)
import qualified Data.Map as M

type Exp = [Term]
type Key = String

data Term
  = V Double
  -- Binary
  | Plus
  | Sub
  | Mul
  | Div
  -- Unary
  | Sin | Cos
  | Log
  | Min | Max
  | Abs
  | Mod
  | Step
  -- Constant
  | PI
  -- User Defined Constant
  | Const Key
  deriving (Show, Read, Eq)

parse :: String -> Either String Exp
parse = mapM work . words
  where
    work a =
      case toTerm a of
        Nothing -> Left $ "Undefined expression: " ++ a
        Just e  -> Right e

toTerm :: String -> Maybe Term
toTerm "+"       = return Plus
toTerm "-"       = return Sub
toTerm "*"       = return Mul
toTerm "/"       = return Div
toTerm "sin"     = return Sin
toTerm "cos"     = return Cos
toTerm "min"     = return Min
toTerm "max"     = return Max
toTerm ('$':key) = return $ Const key
toTerm num       = V <$> readMay num

eval :: M.Map Key Double -> Exp -> Either String Double
eval vmap ts = do
  as <- foldlM go [] ts
  case headMay as of
    Nothing  -> Left "empty expression"
    Just ret -> case ret of
      V r -> Right r
      _   -> Left $ "rest: " ++ show as
  where
    go :: [Term] -> Term -> Either String [Term]
    go (V x:V y:ys) Plus  = return $ V (y + x) : ys
    go (V x:V y:ys) Sub   = return $ V (y - x) : ys
    go (V x:V y:ys) Div
      | y == 0    = Left "divide by zero"
      | otherwise = return $ V (x / y) : ys
    go (V x:V y:ys) Mul   = return $ V (y * x) : ys
    go (V x:ys)     Sin   = return $ V (sin x) : ys
    go (V x:ys)     Cos   = return $ V (cos x) : ys
    go (V x:ys)     Log   = return $ V (log x) : ys
    go (V x:V y:ys) Min   = return $ V (y `min` x) : ys
    go (V x:V y:ys) Max   = return $ V (y `max` x) : ys
    go (V x:ys)     Abs   = return $ V (abs x) : ys
    go (V x:V y:ys) Mod   = return $ V (y `mod'` x) : ys
    go (V x:V y:ys) Step  = return $ V (if y >= x then 1 else 0) : ys
    go xs           PI    = return $ V pi : xs
    go xs           n@V{} = return $ n:xs
    go xs           (Const key) =
      case M.lookup key vmap of
        Nothing -> Left $ "undefined constant value for key: $" ++ key
        Just v  -> go xs (V v)
    go xs           t     = Left $ "eval failed: " ++ show xs ++ " - " ++ show t
