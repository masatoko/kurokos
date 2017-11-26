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

mkGamepad :: Maybe Joystick -> Metapad Action
mkGamepad mjs = mconcat [keyboard, joystick, mouse, touch]
  where
    keyboard = K.metapadFromList
      [ K.released SDL.ScancodeF 0 Go
      , K.pressed SDL.ScancodeReturn 0 Enter
      , K.pressed SDL.ScancodeEscape 0 Exit
      ]

    joystick =
      case mjs of
        Nothing -> mempty
        Just js ->
          K.metapadFromList $
            [ K.joyPressed js 4 Enter
            -- Axes
            , K.joyAxis2 js 0 1 AxisLeft
            -- Hat
            , K.joyHat K.HDUp K.Pressed PUp
            , K.joyHat K.HDUp K.Released RUp
            , K.joyHat K.HDUp K.Holded HUp
            , K.joyHat K.HDDown K.Pressed PDown
            ]
            ++ map (uncurry (K.joyPressed js)) [ (10, Go), (11, Go), (12, Go), (13, Go) ] -- Buttons

    mouse = K.metapadFromList
      [ K.mouseButtonAct K.ButtonLeft K.Pressed 0 Go
      , K.mousePosAct MousePos
      , K.mouseMotionAct MouseMotion
      , K.mouseWheelAct MouseWheel
      ]

    touch = K.metapadFromList [K.touchMotionAct TouchMotion]

-- mkGamepad :: Maybe Joystick -> Metapad Action
-- mkGamepad mjs = flip execState newPad $ do
--   -- Keyboard
--   modify . addAction $ K.released SDL.ScancodeF Go
--   modify . addAction $ K.pressed SDL.ScancodeReturn Enter
--   modify . addAction $ K.pressed SDL.ScancodeEscape Exit
--   -- Joystick
--   case mjs of
--     Just js -> do
--       -- Buttons
--       modify . addAction $ K.joyPressed js 4 Enter
--       mapM_ (modify . addAction . uncurry (K.joyPressed js))
--         [ (10, Go), (11, Go), (12, Go), (13, Go) ]
--       -- Axes
--       modify . addAction $ K.joyAxis2 js 0 1 AxisLeft
--       -- Hat
--       modify . addAction $ K.joyHat K.HDUp K.Pressed PUp
--       modify . addAction $ K.joyHat K.HDUp K.Released RUp
--       modify . addAction $ K.joyHat K.HDUp K.Holded HUp
--       modify . addAction $ K.joyHat K.HDDown K.Pressed PDown
--     Nothing -> return ()
--   -- Mouse
--   modify . addAction $ K.mouseButtonAct K.ButtonLeft K.Pressed Go
--   modify . addAction $ K.mousePosAct MousePos
--   modify . addAction $ K.mouseMotionAct MouseMotion
--   modify . addAction $ K.mouseWheelAct MouseWheel
--   -- Touch
--   modify . addAction $ K.touchMotionAct TouchMotion
