module Kurokos.UI.Widget.Make where

import qualified Control.Exception     as E
import           Control.Lens
import           Control.Monad.Extra   (whenJust)
import           Control.Monad.State
import qualified Data.ByteString       as BS
import qualified Data.Map              as M
import           Data.Text
import qualified Data.Text             as T
import           Linear.V3
import           System.IO             (IOMode (..), hClose, openFile)

import qualified SDL

import qualified Kurokos.Asset         as Asset
import qualified Kurokos.Asset.Raw     as Asset

import qualified Kurokos.Graphics      as G
import qualified Kurokos.Graphics.Font as Font
import           Kurokos.UI.Core
import           Kurokos.UI.Import
import           Kurokos.UI.Widget

newTransparent :: Monad m => GuiT m Widget
newTransparent = return Transparent

newFill :: Monad m => GuiT m Widget
newFill = return Fill

newLabel :: MonadIO m => Asset.Ident -> G.FontSize -> Text -> GuiT m Widget
newLabel ident size title = do
  font <- getFont ident
  return $ Label title font size

newImageView :: MonadIO m => Asset.Ident -> GuiT m Widget
newImageView ident = ImageView <$> getTexture ident

newButton :: MonadIO m => Asset.Ident -> G.FontSize -> Text -> GuiT m Widget
newButton ident size title = do
  font <- getFont ident
  return $ Button title font size

newSwitch :: MonadIO m => Asset.Ident -> G.FontSize -> Text -> GuiT m Widget
newSwitch ident size title = do
  font <- getFont ident
  return $ Switch title font size True

newSlider :: MonadIO m => Asset.Ident -> G.FontSize -> Text -> Value -> GuiT m Widget
newSlider ident size title value = do
  font <- getFont ident
  return $ Slider title font size Nothing value

-- Internal

getTexture :: MonadIO m => Asset.Ident -> GuiT m G.Texture
getTexture ident = do
  ast <- asks geAssetManager
  case Asset.lookupTexture ident ast of
    Nothing  -> liftIO $ E.throwIO $ userError $ "Missing texture: " ++ T.unpack ident
    Just tex -> return tex

getFont :: MonadIO m => Asset.Ident -> GuiT m Font.Font
getFont ident = do
  ast <- asks geAssetManager
  case Asset.lookupFont ident ast of
    Nothing   -> liftIO $ E.throwIO $ userError $ "Missing font: " ++ T.unpack ident
    Just font -> return font
