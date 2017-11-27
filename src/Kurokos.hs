module Kurokos
  (
    KurokosData
  , KurokosEnv
  , KurokosT
  , KurokosEnvT
  , runKurokos
  , runKurokosEnvT
  , withKurokos
  , runScene
  , Update, Render, Transit
  , Config (..), DebugJoystick (..)
  , defaultConfig
  , printsys
  , Scene (..)
  , SceneState (..)
  , Transition
  , continue, end, next, push
  --
  , getEnv
  , screenSize
  , getWindow
  , getEvents
  , getJoysticks
  , averageTime
  , showMessageBox
  , withRenderer
  , setRendererDrawBlendMode
  -- Types
  , Font
  , Joystick (..)
  -- Render
  , setColor
  , clearBy
  , printTest
  -- Font
  , loadFont, freeFont, withFont, withFontB
  -- Sprite
  , allocTexture
  , allocTextureB
  , setBlendMode, setAlphaMod, setColorMod
  -- Metapad
  , Metapad
  , metapadFromList
  , addAction
  , Input
  , MouseButton (..)
  , InputMotion (..)
  , HatDir (..)
  , hold, pressed, released
  , mousePosAct
  , mouseMotionAct
  , mouseButtonAct
  , mouseWheelAct
  , touchMotionAct
  , numAxes, axisPosition
  , joyHold, joyPressed, joyReleased
  , joyAxis, joyAxis2
  , joyAxisChanged, joyAxisChanged2
  -- , joyAllButtons
  -- , joyAllAxes
  , joyHat
  -- , joyAllHat
  , rumble
  ) where

import           Kurokos.Core
import           Kurokos.Types
import           Kurokos.Font
import           Kurokos.Metapad
import           Kurokos.Render
import           Kurokos.Texture
