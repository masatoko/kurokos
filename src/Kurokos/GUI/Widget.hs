{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Widget where

import           Data.Text (Text)
import qualified Data.Text as T

import qualified SDL
import           SDL.Font  (Font)
import qualified SDL.Font  as Font

data Widget
  = Label
    { wTitle :: T.Text
    , wFont  :: Font.Font
    }

instance Show Widget where
  show Label{..} = "<" ++ T.unpack wTitle ++ ">"
