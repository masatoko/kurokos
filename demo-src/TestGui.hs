{-# LANGUAGE OverloadedStrings #-}
module TestGui where

import           Control.Monad.IO.Class   (liftIO)

import qualified Kurokos                  as K
import           Kurokos.GUI.Core
import qualified Kurokos.GUI.Widget.Label as Label

testGui :: K.Font -> IO GuiState
testGui font = do
  gst <- newGui env $ do
    label1 <- genSingle =<< Label.newLabel "test1"
    label2 <- genSingle =<< Label.newLabel "test2"
    cnt <- genContainer [label1, label2]
    putWT cnt
  let wt = getWidgetTree gst
  print wt
  return gst
  where
    env = GuiEnv font
