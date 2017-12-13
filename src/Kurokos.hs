module Kurokos
  (
    KurokosData
  , KurokosEnv
  , KurokosT
  , runKurokos
  , withKurokos
  , runScene
  , Update, Render, Transit
  , KurokosConfig (..)
  , printsys
  , Scene (..)
  , SceneState (..)
  , Transition (..)
  , continue
  , next
  , push
  , end
  --
  , screenSize
  , getWindow
  , getEvents
  , getJoysticks
  , averageTime
  , showMessageBox
  , getRenderer
  , withRenderer
  , setRendererDrawBlendMode
  -- Types
  , Joystick (..)
  -- Render
  , setColor
  , clearBy
  , printTest
  -- Sprite
  , allocTexture
  , allocTextureB
  , setBlendMode
  , setAlphaMod
  , setColorMod
  ) where

import           Kurokos.Core
import           Kurokos.Types
import           Kurokos.Render
import           Kurokos.Texture
