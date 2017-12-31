module Kurokos.Graphics.Matrix where

import Linear

mkOrtho :: Integral a => V2 a -> Bool -> M44 Float
mkOrtho (V2 iw ih) vertFlip
  | vertFlip  = ortho 0 w (-h) 0 (-1) 1
  | otherwise = ortho 0 w 0    h 1    (-1)
  where
    w = fromIntegral iw
    h = fromIntegral ih
