{-# LANGUAGE OverloadedStrings #-}
module Kurokos.GUI.Test where

import Control.Monad.IO.Class (liftIO)

import qualified Kurokos.GUI.Widget.Label as Label
import Kurokos.GUI.Core

testGui :: IO ()
testGui = do
  gst <- newGui $ do
    label1 <- genSingle $ Label.Label "test1"
    label2 <- genSingle $ Label.Label "test2"
    cnt <- genContainer [label1, label2]
    putWT cnt
  let wt = getWidgetTree gst
  print wt
