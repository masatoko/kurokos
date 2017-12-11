{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Managed  (runManaged) -- managed
import           System.Environment     (getArgs)

import           SDL                    (($=))
import qualified SDL

import qualified Kurokos                as K

import           Import

import           Scene

main :: IO ()
main = do
  as <- getArgs
  let opt = (`elem` as)
      conf = mkConf (opt "button") (opt "axis") (opt "hat") -- TODO: fix mkConf
      -- conf' = conf {K.confFont = Left fontBytes}
      conf' = conf {K.confFont = K.FontFile "_data/font/system.ttf"}
      winConf = SDL.defaultWindow
        { SDL.windowInitialSize = V2 640 480
        , SDL.windowMode = SDL.Windowed
        , SDL.windowResizable = True
        }
  withKurokos conf' winConf $ \kuro ->
    -- Ready original data here
    runManaged $
      -- _font <- managed $ K.withFont (K.FontFile "_data/font/system.ttf") 20 -- Example
      liftIO $ void $
        runKurokos kuro $ do
          -- === SDL Settings
          SDL.setMouseLocationMode SDL.AbsoluteLocation
          -- SDL.setMouseLocationMode SDL.RelativeLocation
          SDL.cursorVisible $= True
          -- ===
          runScene titleScene
  where
    mkConf pBtn pAxis pHat =
      K.defaultConfig
        { K.confWinTitle = "kurokos"
        , K.confDebugPrintSystem = True
        , K.confDebugJoystick = K.DebugJoystick pBtn pAxis pHat
        }
