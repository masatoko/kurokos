module Kurokos.GUI.Widget.Label where

import           Data.Text          (Text)
import qualified Data.Text          as T

import qualified SDL

import           Kurokos.GUI.Import

import           Kurokos.GUI.Core
import           Kurokos.Types      (Font, RenderEnv (..))

data Label = Label
  { title :: Text
  , font  :: Font
  } deriving Show

instance Widget Label where
  showW a = "[" ++ T.unpack (title a) ++ "]"
  render = render_

newLabel :: Monad m => Text -> GuiT m Label
newLabel title = do
  font <- asks geFont
  return $ Label title font

render_ :: RenderEnv m => Label -> m ()
render_ label =
  renderText $ title label
