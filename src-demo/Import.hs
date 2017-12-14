module Import
  ( Int16
  , Int32
  , CInt
  , V2(..)
  , V4(..)
  , (^*)
  , Point(..)
  --
  , (<>)
  --
  , ResourceT
  , allocate
  --
  , Joystick
  , KurokosT
  , Update
  , Render
  , Scene (..)
  , withKurokos
  , runKurokos
  , runScene
  ) where

import           Control.Monad.Trans.Resource (ResourceT, allocate)
import           Data.Int                     (Int16, Int32)
import           Data.Monoid                  ((<>))
import           Foreign.C.Types              (CInt)

import           SDL                          (Point (..))
import           SDL.Vect                     (V2 (..), V4 (..), (^*))

import           Kurokos                      (Joystick, KurokosT, Render,
                                               Scene (..), Update, runKurokos,
                                               runScene, withKurokos)
