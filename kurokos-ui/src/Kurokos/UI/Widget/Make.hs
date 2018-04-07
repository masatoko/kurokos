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

mkTransparent :: Widget
mkTransparent = Transparent

mkFill :: Widget
mkFill = Fill

mkLabel :: T.Text -> Widget
mkLabel = Label

newImageView :: MonadIO m => Asset.Ident -> GuiT m Widget
newImageView ident = ImageView <$> getTexture ident

mkButton :: T.Text -> Widget
mkButton = Button

mkSwitch :: T.Text -> Widget
mkSwitch title = Switch title True

mkSlider :: T.Text -> Value -> Widget
mkSlider title = Slider title Nothing

mkTextField :: T.Text -> Widget
mkTextField iniText = TextField z Nothing
  where
    z = TZ.textZipper [iniText] Nothing

mkPicker :: [(String, T.Text)] -> Maybe String -> Widget
mkPicker ts mDefKey = Picker ts idx []
  where
    idx = case mDefKey of
            Just defkey -> fromMaybe 0 $ findIndex ((== defkey) . fst) ts
            Nothing     -> 0

-- Internal

getTexture :: MonadIO m => Asset.Ident -> GuiT m G.Texture
getTexture ident = do
  ast <- asks geAssetManager
  case Asset.lookupTexture ident ast of
    Nothing  -> liftIO $ E.throwIO $ userError $ "Missing texture: " ++ T.unpack ident
    Just tex -> return tex
