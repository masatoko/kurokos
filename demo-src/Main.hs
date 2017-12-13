{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
-- import           Control.Monad.Managed  (runManaged)
import qualified Data.ByteString        as BS

import           SDL                    (($=))
import qualified SDL

import           Kurokos                (KurokosConfig (..))
import qualified Kurokos                as K
import qualified Kurokos.Asset          as Asset

import           Import
import           Scene

main :: IO ()
main = do
  astMgr <- Asset.loadAssetManager =<< Asset.decodeAssetList =<< BS.readFile "_data/system-assets.yaml"
  let conf = K.KurokosConfig
        { confWinTitle         = "Kurokos Demo"
        , confDebugPrintFPS    = True
        , confDebugPrintSystem = True
        , confSystemAsset      = astMgr
        , confSystemFontId     = "_system-font"
        }
  withKurokos conf winConf $ \kuro ->
    -- Allocate original data here
      liftIO $ void $
        runKurokos kuro $ do
          -- === SDL Settings
          SDL.setMouseLocationMode SDL.AbsoluteLocation
          -- SDL.setMouseLocationMode SDL.RelativeLocation
          SDL.cursorVisible $= True
          -- ===
          runScene titleScene
  where
    winConf = SDL.defaultWindow
      { SDL.windowInitialSize = V2 640 480
      , SDL.windowMode = SDL.Windowed
      , SDL.windowResizable = True
      }
