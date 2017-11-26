module Import
  ( Int16, Int32
  , CInt
  --
  , (<>)
  --
  , V2(..)
  , V4(..)
  , (^*)
  , Point(..)
  --
  , ResourceT
  , allocate
  --
  , Joystick
  , KurokosT
  , Metapad
  , Update
  , Render
  , Scene (..)
  , SceneState (..)
  , withKurokos
  , runKurokos
  , runScene
  , metapadFromList
  , addAction
  ) where

import           Control.Monad.Trans.Resource (ResourceT, allocate)
import           Data.Int                     (Int16, Int32)
import           Foreign.C.Types              (CInt)
import           Data.Monoid                  ((<>))
import           Linear.Affine
import           Linear.V2                    (V2 (..))
import           Linear.V4                    (V4 (..))
import           Linear.Vector                ((^*))

import           Kurokos                      (Joystick, KurokosT, Metapad,
                                               Render, Scene (..),
                                               SceneState (..), Update,
                                               addAction, metapadFromList,
                                               runKurokos, runScene,
                                               withKurokos)

import           SDL                          (Point (..))
