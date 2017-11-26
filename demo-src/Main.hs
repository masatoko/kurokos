{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception.Safe as E
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.ByteString              as B
import           Data.Int                     (Int16, Int32)
import           System.Environment           (getArgs)

import qualified SDL

import qualified Kurokos                      as K

import Import

import Pad
import Scene

main :: IO ()
main = do
  as <- getArgs
  let opt = (`elem` as)
      conf = mkConf (opt "button") (opt "axis") (opt "hat")
      -- conf' = conf {K.confFont = Left fontBytes}
      conf' = conf {K.confFont = Right "_data/system.ttf"}
  withKurokos conf' $ \kuro -> do
    mjs <- K.newJoystickAt 0
    let gamepad = mkGamepad mjs
    _ <- runKurokos kuro $
      runScene $ titleScene mjs gamepad
    maybe (return ()) K.freeJoystick mjs
    return ()
  where
    mkConf pBtn pAxis pHat =
      K.defaultConfig
        { K.confWinSize = V2 640 480
        , K.confWinTitle = "protpnic-app"
        -- , K.confWindowMode = SDL.Fullscreen
        , K.confWindowMode = SDL.Windowed
        , K.confDebugPrintSystem = True
        , K.confDebugJoystick = K.DebugJoystick pBtn pAxis pHat
        }

    -- monitor mjs =
    --   case mjs of
    --     Nothing -> return ()
    --     Just js -> forever $ do
    --       clearScreen
    --       SDL.pumpEvents
    --       K.monitorJoystick js
    --       threadDelay 100000
