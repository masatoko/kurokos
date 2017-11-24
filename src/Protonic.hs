module Protonic
  (
    Proto
  , ProtoConfig
  , ProtoT
  , ProtoConfT
  , runProtoT
  , runProtoConfT
  , withProtonic
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
  , getProtoConfig
  , screenSize
  , getWindow
  , averageTime
  , showMessageBox
  , setRendererDrawBlendMode
  -- Data
  , Font
  , Sprite (spsize)
  -- Render
  , setColor
  , clearBy
  , drawLine, drawRect, fillRect
  , renderS, renderS'
  , printTest
  -- Font
  , newFont, freeFont, withFont
  -- Sprite
  , newSprite, loadSprite, decodeSprite, freeSprite
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

import           Protonic.Core
import           Protonic.Data
import           Protonic.Font_
import           Protonic.Metapad
import           Protonic.Render
import           Protonic.Sprite
