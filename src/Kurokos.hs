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
  , averageTime
  , showMessageBox
  , withRenderer
  , setRendererDrawBlendMode
  -- Data
  , Font
  , Sprite (spsize)
  -- Render
  , setColor
  , clearBy
  -- , drawLine, drawRect, fillRect
  -- , renderS, renderS'
  , printTest
  -- Font
  , loadFont, freeFont, withFont
  -- Sprite
  , allocTexture
  , loadSprite, decodeSprite, freeSprite
  , setBlendMode, setAlphaMod, setColorMod
  -- Metapad
  , Metapad
  , newPad
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
  , Joystick
  , monitorJoystick
  , newJoystickAt, freeJoystick
  , numAxes, axisPosition
  , joyHold, joyPressed, joyReleased
  , joyAxis, joyAxis2
  , joyAxisChanged, joyAxisChanged2
  , joyAllButtons
  , joyAllAxes
  , joyHat
  , joyAllHat
  , rumble
  ) where

import           Kurokos.Core
import           Kurokos.Data
import           Kurokos.Font
import           Kurokos.Metapad
import           Kurokos.Render
import           Kurokos.Sprite
