{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception.Safe as E
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Managed  (Managed, managed, runManaged)
import qualified Data.ByteString        as B
import           Data.Int               (Int16, Int32)
import           System.Environment     (getArgs)

import           SDL                    (($=))
import qualified SDL

import qualified Kurokos                as K

import           Import

import           Pad
import           Scene

import           TestGui                (testGui)

main :: IO ()
main = do
  as <- getArgs
  let opt = (`elem` as)
      conf = mkConf (opt "button") (opt "axis") (opt "hat") -- TODO: fix mkConf
      -- conf' = conf {K.confFont = Left fontBytes}
      conf' = conf {K.confFont = K.FontFile "_data/system.ttf"}
  withKurokos conf' $ \kuro ->
    -- Ready original data here
    runManaged $ do
      font <- managed $ K.withFont (K.FontFile "_data/system.ttf") 20
      liftIO $ do
        testGui font
        runKurokos kuro $ do
          SDL.setMouseLocationMode SDL.AbsoluteLocation
          SDL.cursorVisible $= False
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
