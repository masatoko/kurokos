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
      conf = mkConf (opt "button") (opt "axis") (opt "hat") -- TODO: fix mkConf
      -- conf' = conf {K.confFont = Left fontBytes}
      conf' = conf {K.confFont = Right "_data/system.ttf"}
  withKurokos conf' $ \kuro -> do
    runKurokos kuro $
      runScene titleScene
    return ()
  where
    mkConf pBtn pAxis pHat =
      K.defaultConfig
        { K.confWinSize = V2 640 480
        , K.confWinTitle = "protpnic-app"
        , K.confWindowMode = SDL.Windowed
        , K.confDebugPrintSystem = True
        , K.confDebugJoystick = K.DebugJoystick pBtn pAxis pHat
        }
