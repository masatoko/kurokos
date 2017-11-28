{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception.Safe as E
import           Control.Monad          (void)
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

main :: IO ()
main = do
  as <- getArgs
  let opt = (`elem` as)
      conf = mkConf (opt "button") (opt "axis") (opt "hat") -- TODO: fix mkConf
      -- conf' = conf {K.confFont = Left fontBytes}
      conf' = conf {K.confFont = K.FontFile "_data/system.ttf"}
      winConf = SDL.defaultWindow
        { SDL.windowInitialSize = V2 640 480
        , SDL.windowMode = SDL.Windowed
        , SDL.windowResizable = True
        }
  withKurokos conf' winConf $ \kuro ->
    -- Ready original data here
    runManaged $ do
      _font <- managed $ K.withFont (K.FontFile "_data/system.ttf") 20 -- Test
      liftIO $ void $
        runKurokos kuro $ do
          SDL.setMouseLocationMode SDL.AbsoluteLocation
          SDL.cursorVisible $= True
          runScene titleScene
  where
    mkConf pBtn pAxis pHat =
      K.defaultConfig
        { K.confWinTitle = "protpnic-app"
        , K.confDebugPrintSystem = True
        , K.confDebugJoystick = K.DebugJoystick pBtn pAxis pHat
        }
