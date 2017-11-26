module Kurokos.GUI.Widget.Label where

import           Data.Text        (Text)
import qualified Data.Text        as T

import           Kurokos.GUI.Core

data Label = Label
  { title :: Text
  } deriving Show

instance Widget Label where
  showW label = "[" ++ T.unpack (title label) ++ "]"
