{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Kurokos.Graphics.Camera
  ( Camera
  , camCoord
  , camHeight
  , camUp
  , mkCamera
  , camForVertFlip
  -- , mkCameraVertFlip
  , viewMatFromCam
  ) where

import           Control.Lens
import           Kurokos.Graphics.Vect
import           Linear.Matrix
import           Linear.Projection

data Camera = Camera
  { _camCoord  :: V2 Float
  , _camHeight :: Float
  , _camUp     :: V3 Float
  } deriving (Eq, Show, Read)

makeLenses ''Camera

mkCamera :: Camera
mkCamera = Camera (pure 0) 1 (V3 0 1 0)

camForVertFlip :: Camera
camForVertFlip = Camera (pure 0) (-1) (V3 0 (-1) 0)

viewMatFromCam :: Camera -> M44 Float
viewMatFromCam cam =
  lookAt eye center (cam^.camUp)
  where
    V2 x y = cam^.camCoord
    h = cam^.camHeight
    --
    eye = V3 x y h
    center = V3 x y 0
