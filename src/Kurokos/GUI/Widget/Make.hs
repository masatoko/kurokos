module Kurokos.GUI.Widget.Make where

import           Data.Text

import qualified SDL
import qualified SDL.Font           as Font
import qualified SDL.Image          as Image

import           Kurokos.GUI.Core
import           Kurokos.GUI.Import
import           Kurokos.GUI.Widget

newTransparent :: Monad m => GuiT m Widget
newTransparent = return Transparent

newFill :: Monad m => GuiT m Widget
newFill = return Fill

newLabel :: (RenderEnv m, MonadIO m) => Text -> GuiT m Widget
newLabel title = do
  font <- loadDefaultFont
  return Label
    { wTitle = title
    , wFont = font
    }

newImageView :: (RenderEnv m, MonadIO m) => FilePath -> GuiT m Widget
newImageView texPath = do
  load <- asks geFileLoader
  byte <- liftIO $ load texPath
  tex <- lift $ withRenderer $ \r -> Image.decodeTexture r byte
  return $ ImageView tex

newButton :: (RenderEnv m, MonadIO m) => Text -> GuiT m Widget
newButton title = do
  font <- loadDefaultFont
  return Button
    { wTitle = title
    , wFont = font
    }

-- Helper

loadDefaultFont :: (RenderEnv m, MonadIO m) => GuiT m Font.Font
loadDefaultFont = do
  path <- asks geDefaultFontPath
  loadFont path 16
