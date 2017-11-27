module Kurokos.GUI.Widget where

import           Data.Text (Text)
import qualified Data.Text as Text

import qualified SDL
import           SDL.Font  (Font)
import qualified SDL.Font  as Font

data Widget
  = Label
    { wTitle :: Text.Text
    , wFont  :: Font.Font
    }
  deriving Show
