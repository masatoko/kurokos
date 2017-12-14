{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Exception as E
import           Control.Monad     (void)
import qualified Data.ByteString   as BS

import           SDL               (($=))
import qualified SDL

import           Kurokos           (KurokosConfig (..))
import qualified Kurokos           as K
import qualified Kurokos.Asset     as Asset
import qualified Kurokos.Asset.SDL as Asset

import           Game
import           Import
import           Scene             (startScene)

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
  withKurokos conf winConf $ \kuro -> do
    -- Allocate original data here
    r <- runKurokos kuro K.getRenderer
    globalAssets <- Asset.newSDLAssetManager r =<< Asset.loadAssetManager =<< Asset.decodeAssetList =<< BS.readFile "_data/assets-global.yaml"
    E.handle handler $
      void $ runGameT (GameEnv globalAssets) GameState $
        runKurokos kuro $ do
          SDL.setMouseLocationMode SDL.AbsoluteLocation
          -- SDL.setMouseLocationMode SDL.RelativeLocation
          SDL.cursorVisible $= True
          -- ===
          startScene
  where
    winConf = SDL.defaultWindow
      { SDL.windowInitialSize = V2 640 480
      , SDL.windowMode = SDL.Windowed
      , SDL.windowResizable = True
      }

    handler :: K.KurokosException -> IO ()
    handler e = putStrLn $ "Caught exception: " ++ show e
