{-# LANGUAGE RecordWildCards #-}
module Kurokos.GUI.Widget.Label where

import qualified Control.Exception.Safe as E
import           Data.Text              (Text)
import qualified Data.Text              as T

import qualified SDL
import qualified SDL.Font               as Font

import           Kurokos.GUI.Import

import           Kurokos.GUI.Core
import           Kurokos.GUI.Def        (RenderEnv (..))
import           Kurokos.Types          (Font)

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

render_ :: (MonadIO m, RenderEnv m, MonadMask m) => Label -> m ()
render_ Label{..} = do
  tex <- withRenderer $ \r ->
    E.bracket
      (Font.blended font (V4 255 255 255 255) title)
      SDL.freeSurface
      (SDL.createTextureFromSurface r) -- TODO: Must create once!
  renderTexture tex $ Rectangle (P $ V2 0 0) (V2 50 50)
