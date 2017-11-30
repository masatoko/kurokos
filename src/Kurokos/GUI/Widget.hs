{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Widget where

import           Data.Text (Text)
import qualified Data.Text as T

import qualified SDL
import           SDL.Font  (Font)

data Widget
  = Transparent
  | Label
    { wTitle :: Text
    , wFont  :: Font
    }
  | ImageView
    { wImage :: SDL.Texture
    }
  | Button
    { wTitle :: Text
    , wFont  :: Font
    }

instance Show Widget where
  show Transparent   = "<T>"
  show Label{..}     = "<L:" ++ T.unpack wTitle ++ ">"
  show ImageView{..} = "<IMG>"
  show Button{..}    = "<B:" ++ T.unpack wTitle ++ ">"

class Hoverable a where
  hoverable :: a -> Bool

instance Hoverable Widget where
  hoverable Transparent = False
  hoverable Label{}     = False
  hoverable ImageView{} = True
  hoverable Button{}    = True
