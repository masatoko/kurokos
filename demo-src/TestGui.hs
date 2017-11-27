{-# LANGUAGE OverloadedStrings #-}
module TestGui where

import           Control.Monad.IO.Class   (liftIO)

import qualified Kurokos                  as K

-- Temporal
import           Kurokos.GUI.Core
import qualified Kurokos.GUI.Widget.Label as Label
-- Temporal

testGui :: K.Font -> IO GuiState
testGui font = do
  gst <- newGui env $ do
    label1 <- genSingle =<< Label.newLabel "label1"
    label2 <- genSingle =<< Label.newLabel "label2"
    cnt <- genContainer [label1, label2]
    putWT cnt
  print $ getWidgetTree gst
  return gst
  where
    env = GuiEnv font
