module Kurokos.UI.Widget.Make where

import qualified Control.Exception   as E
import           Control.Lens
import           Control.Monad.State
import qualified Data.ByteString     as BS
import qualified Data.Map            as M
import           Data.Text
import qualified Data.Text           as T
import           System.IO           (IOMode (..), hClose, openFile)

import qualified SDL
import qualified SDL.Font            as Font
import qualified SDL.Image           as Image

import qualified Kurokos.Asset       as Asset
import qualified Kurokos.Asset.SDL   as Asset

import           Kurokos.UI.Core
import           Kurokos.UI.Import
import           Kurokos.UI.Widget

newTransparent :: Monad m => GuiT m Widget
newTransparent = return Transparent

newFill :: Monad m => GuiT m Widget
newFill = return Fill

newLabel :: MonadIO m
  => Asset.Ident -> Text -> Font.PointSize -> GuiT m Widget
newLabel ident title size = do
  font <- getFont ident size
  return Label
    { wTitle = title
    , wFont = font
    }

newImageView :: MonadIO m
  => Asset.Ident -> GuiT m Widget
newImageView ident = do
  tex <- getTexture ident
  return $ ImageView tex

newButton :: MonadIO m
  => Asset.Ident -> Text -> Font.PointSize -> GuiT m Widget
newButton ident title size = do
  font <- getFont ident size
  return Button
    { wTitle = title
    , wFont = font
    }

getTexture :: MonadIO m => Asset.Ident -> GuiT m SDL.Texture
getTexture ident = do
  ast <- asks geAssetManager
  case Asset.lookupTexture ident ast of
    Nothing   -> liftIO $ E.throwIO $ userError $ "missing texture: " ++ T.unpack ident
    Just font -> return font

getFont :: MonadIO m => Asset.Ident -> Font.PointSize -> GuiT m Font.Font
getFont ident size = do
  ast <- asks geAssetManager
  lift $ Asset.getFont ident size ast
