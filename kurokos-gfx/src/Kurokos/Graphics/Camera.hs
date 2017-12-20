{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Kurokos.Graphics.Camera
  ( Camera
  , camCoord
  , camHeight
  , mkCamera
  , viewMatFromCam
  ) where

import Control.Lens
import Linear

data Camera = Camera
  { _camCoord  :: V2 Float
  , _camHeight :: Float
  } deriving (Eq, Show, Read)

makeLenses ''Camera

mkCamera :: Camera
mkCamera = Camera (pure 0) 1

viewMatFromCam :: Camera -> M44 Float
viewMatFromCam cam = lookAt eye center up
  where
    V2 x y = cam^.camCoord
    h = cam^.camHeight
    --
    eye = V3 x y h
    center = V3 x y (h - 1)
    up = V3 0 1 0
