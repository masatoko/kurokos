module Kurokos.UI.Widget.Make where

import qualified Control.Exception.Safe as E
import           Control.Lens
import           Control.Monad.State
import qualified Data.ByteString        as BS
import qualified Data.Map               as M
import           Data.Text
import qualified Data.Text              as T
import           System.IO              (IOMode (..), hClose, openFile)

import qualified SDL
import qualified SDL.Font               as Font
import qualified SDL.Image              as Image

import qualified Kurokos.Asset          as Asset
import qualified Kurokos.Asset.SDL      as Asset

import           Kurokos.UI.Core
import           Kurokos.UI.Import
import           Kurokos.UI.Widget

newTransparent :: Monad m => GuiT m Widget
newTransparent = return Transparent

newFill :: Monad m => GuiT m Widget
newFill = return Fill

newLabel :: (RenderEnv m, MonadIO m, MonadResource m)
  => Asset.Ident -> Text -> GuiT m Widget
newLabel ident title = do
  font <- getFont ident 16
  return Label
    { wTitle = title
    , wFont = font
    }

newImageView :: (RenderEnv m, MonadIO m, MonadResource m)
  => Asset.Ident -> GuiT m Widget
newImageView ident = do
  tex <- getTexture ident
  return $ ImageView tex

newButton :: (RenderEnv m, MonadIO m, MonadResource m)
  => Asset.Ident -> Text -> GuiT m Widget
newButton ident title = do
  font <- getFont ident 16
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

getFont :: MonadIO m => Asset.Ident -> Int -> GuiT m Font.Font
getFont ident size = do
  fontMap <- use gFontMap
  case M.lookup (ident, size) fontMap of
    Just font -> return font
    Nothing   -> do
      ast <- asks geAssetManager
      case Asset.lookupFont ident ast of
        Nothing   -> liftIO $ E.throwIO $ userError $ "missing font: " ++ T.unpack ident
        Just bytes -> do
          font <- Font.decode bytes size
          modify' $ over gFontMap $ M.insert (ident, size) font
          return font

-- allocTexture :: (RenderEnv m, MonadIO m, MonadResource m) => FilePath -> GuiT m SDL.Texture
-- allocTexture path = do
--   r <- lift getRenderer
--   asks geFileLoader >>= \case
--     Nothing   -> snd <$> lift (allocate (Image.loadTexture r path) SDL.destroyTexture)
--     Just load ->
--       case load path of
--         Nothing -> E.throw $ userError $ "missing file by file loader: " ++ path
--         Just bs -> snd <$> lift (allocate (Image.decodeTexture r bs) SDL.destroyTexture)

-- allocFont :: (MonadIO m, MonadResource m) => FilePath -> GuiT m Font.Font
-- allocFont path = do
--   mLoad <- asks geFileLoader
--   case mLoad of
--     Nothing   -> snd <$> lift (allocate (Font.load path 16) Font.free)
--     Just load ->
--       case load path of
--         Nothing -> E.throw $ userError $ "missing file by file loader: " ++ path
--         Just bs -> snd <$> lift (allocate (Font.decode bs 16) Font.free)
