{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Widget where

import           Data.Text (Text)
import qualified Data.Text as T

import           SDL.Font  (Font)

data Widget
  = Label
    { wTitle :: Text
    , wFont  :: Font
    }
  | Button
    { wTitle :: Text
    , wFont  :: Font
    }

instance Show Widget where
  show Label{..} = "<L:" ++ T.unpack wTitle ++ ">"
  show Button{..} = "<B:" ++ T.unpack wTitle ++ ">"

class Hoverable a where
  hoverable :: a -> Bool

instance Hoverable Widget where
  hoverable Label{} = False
  hoverable Button{} = True
