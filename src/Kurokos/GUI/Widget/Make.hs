{-# LANGUAGE LambdaCase #-}
module Kurokos.GUI.Widget.Make where

import qualified Control.Exception.Safe as E
import System.IO (openFile, hClose, IOMode (..))
import           Data.Text
import qualified Data.ByteString as BS

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

newLabel :: (RenderEnv m, MonadIO m, MonadResource m)
  => Text -> GuiT m Widget
newLabel title = do
  font <- allocFont =<< asks geDefaultFontPath
  return Label
    { wTitle = title
    , wFont = font
    }

newImageView :: (RenderEnv m, MonadIO m, MonadResource m)
  => FilePath -> GuiT m Widget
newImageView texPath = do
  tex <- allocTexture texPath
  return $ ImageView tex

newButton :: (RenderEnv m, MonadIO m, MonadResource m)
  => Text -> GuiT m Widget
newButton title = do
  font <- allocFont =<< asks geDefaultFontPath
  return Button
    { wTitle = title
    , wFont = font
    }

-- Helper

allocTexture :: (RenderEnv m, MonadIO m, MonadResource m) => FilePath -> GuiT m SDL.Texture
allocTexture path = do
  r <- lift getRenderer
  asks geFileLoader >>= \case
    Nothing   -> snd <$> lift (allocate (Image.loadTexture r path) SDL.destroyTexture)
    Just load ->
      case load path of
        Nothing -> E.throw $ userError $ "missing file by file loader: " ++ path
        Just bs -> snd <$> lift (allocate (Image.decodeTexture r bs) SDL.destroyTexture)

allocFont :: (MonadIO m, MonadResource m) => FilePath -> GuiT m Font.Font
allocFont path = do
  mLoad <- asks geFileLoader
  case mLoad of
    Nothing   -> snd <$> lift (allocate (Font.load path 16) Font.free)
    Just load ->
      case load path of
        Nothing -> E.throw $ userError $ "missing file by file loader: " ++ path
        Just bs -> snd <$> lift (allocate (Font.decode bs 16) Font.free)
