module Kurokos.UI.Widget.Make where

import qualified Control.Exception     as E
import           Control.Lens
import           Control.Monad.Extra   (whenJust)
import           Control.Monad.State
import qualified Data.ByteString       as BS
import           Data.List             (findIndex)
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import qualified Data.Text.Zipper      as TZ
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

newLabel :: MonadIO m => T.Text -> GuiT m Widget
newLabel title =
  return $ Label title

newImageView :: MonadIO m => Asset.Ident -> GuiT m Widget
newImageView ident = ImageView <$> getTexture ident

newButton :: MonadIO m => Asset.Ident -> GuiT m Widget
newButton title =
  return $ Button title

newSwitch :: MonadIO m => T.Text -> GuiT m Widget
newSwitch title =
  return $ Switch title True

newSlider :: MonadIO m => T.Text -> Value -> GuiT m Widget
newSlider title value =
  return $ Slider title Nothing value

newTextField :: MonadIO m => T.Text -> GuiT m Widget
newTextField iniText =
  return $ TextField z Nothing
  where
    z = TZ.textZipper [iniText] Nothing

newPicker :: MonadIO m => [(String, T.Text)] -> Maybe String -> GuiT m Widget
newPicker ts mDefKey = do
  let idx = case mDefKey of
              Just defkey -> fromMaybe 0 $ findIndex ((== defkey) . fst) ts
              Nothing     -> 0
  return $ Picker ts idx []

-- Internal

getTexture :: MonadIO m => Asset.Ident -> GuiT m G.Texture
getTexture ident = do
  ast <- asks geAssetManager
  case Asset.lookupTexture ident ast of
    Nothing  -> liftIO $ E.throwIO $ userError $ "Missing texture: " ++ T.unpack ident
    Just tex -> return tex
