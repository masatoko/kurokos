module Pad where

import           Control.Monad.State

import qualified SDL

import qualified Kurokos             as K

import           Import

data Action
  = Go
  | Enter
  | Exit
  | AxisLeft Int16 Int16
  | PUp | HUp | RUp
  | PDown
  --
  | MousePos (V2 Int)
  | MouseMotion (V2 Int32)
  | MouseWheel (V2 Int32)
  | TouchMotion (V2 Double)
  deriving (Eq, Show)

defPad :: Metapad Action
defPad = mconcat [keyboard, mouse, touch]
  where
    keyboard = K.metapadFromList
      [ K.released SDL.ScancodeF Go
      , K.pressed SDL.ScancodeReturn Enter
      , K.pressed SDL.ScancodeEscape Exit
      ]

    -- joystick =
    --   case mjs of
    --     Nothing -> mempty
    --     Just js ->
    --       K.metapadFromList $
    --         [ K.joyPressed js 4 Enter
    --         -- Axes
    --         , K.joyAxis2 js 0 1 AxisLeft
    --         -- Hat
    --         , K.joyHat K.HDUp K.Pressed PUp
    --         , K.joyHat K.HDUp K.Released RUp
    --         , K.joyHat K.HDUp K.Holded HUp
    --         , K.joyHat K.HDDown K.Pressed PDown
    --         ]
    --         ++ map (uncurry (K.joyPressed js)) [ (10, Go), (11, Go), (12, Go), (13, Go) ] -- Buttons

    mouse = K.metapadFromList
      [ K.mouseButtonAct K.ButtonLeft K.Pressed Go
      , K.mousePosAct MousePos
      , K.mouseMotionAct MouseMotion
      , K.mouseWheelAct MouseWheel
      ]

    touch = K.metapadFromList [K.touchMotionAct TouchMotion]
