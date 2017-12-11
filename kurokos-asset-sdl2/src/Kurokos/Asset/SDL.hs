{-# LANGUAGE RecordWildCards #-}
module Kurokos.Asset.SDL
  (
  -- ** Types
    SDLAssetManager
  -- ** Generate
  , genSDLAssetManager
  , freeSDLAssetManager
  -- ** Get
  , getFont
  , lookupTexture
  , lookupBytes
  ) where

import           Control.Concurrent.MVar
import qualified Control.Exception            as E
import           Control.Monad                (foldM)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import qualified Data.ByteString              as BS
import           Data.Char                    (toLower)
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import qualified SDL
import           System.FilePath.Posix

import qualified SDL.Font                     as Font
import qualified SDL.Image                    as Image

import           Kurokos.Asset                (Ident)
import qualified Kurokos.Asset                as Asset
import           Kurokos.Asset.Internal.Types (AssetManager (..))

data SDLAssetManager = SDLAssetManager
  { amByteMap    :: M.Map Ident BS.ByteString
  , amFontMap    :: M.Map Ident BS.ByteString
  , amTexMap     :: M.Map Ident SDL.Texture
  --
  , amFontHolder :: MVar (M.Map (Ident, Font.PointSize) Font.Font)
  }

genSDLAssetManager :: MonadIO m => SDL.Renderer -> AssetManager -> m SDLAssetManager
genSDLAssetManager r (AssetManager bmap) = do
  mvFontHolder <- liftIO $ newMVar M.empty
  let empty = SDLAssetManager M.empty M.empty M.empty mvFontHolder
  foldM work empty $ M.toList bmap
  where
    work am@SDLAssetManager{..} (ident, (path, bytes)) = update
      where
        ext = filter (/= '.') . map toLower . takeExtension $ path
        update
          | ext == "ttf" =
              return $ am {amFontMap = M.insert ident bytes amFontMap}
          | ext == "tga" = do
              tex <- Image.decodeTextureTGA r bytes
              return $ am {amTexMap = M.insert ident tex amTexMap}
          | S.member ext imageEtxs = do
              tex <- Image.decodeTexture r bytes
              return $ am {amTexMap = M.insert ident tex amTexMap}
          | otherwise    =
              return $ am {amByteMap = M.insert ident bytes amByteMap}

    imageEtxs :: S.Set String
    imageEtxs = S.fromList ["bmp", "gif", "jpeg", "lbm", "pcx", "png", "pnm", "svg", "tiff", "webp", "xcf", "xpm", "xv"]

freeSDLAssetManager :: MonadIO m => SDLAssetManager -> m ()
freeSDLAssetManager SDLAssetManager{..} = liftIO $ do
  mapM_ SDL.destroyTexture $ M.elems amTexMap
  fontmap <- readMVar amFontHolder
  mapM_ Font.free $ M.elems fontmap

getFont :: MonadIO m => Ident -> Font.PointSize -> SDLAssetManager -> m Font.Font
getFont ident size SDLAssetManager{..} = liftIO $ do
  fontmap <- readMVar amFontHolder
  case M.lookup key fontmap of
    Just font -> return font
    Nothing ->
      case M.lookup ident amFontMap of
        Nothing        -> liftIO $ E.throwIO $ userError $ "Missing font '" ++ T.unpack ident ++ "' From SDLAssetManager"
        Just fontBytes -> do
          font <- Font.decode fontBytes size
          modifyMVar_ amFontHolder $ \fm ->
            return $ M.insert key font fontmap
          return font
  where
    key = (ident, size)

lookupTexture :: Ident -> SDLAssetManager -> Maybe SDL.Texture
lookupTexture ident SDLAssetManager{..} = M.lookup ident amTexMap

lookupBytes :: Ident -> SDLAssetManager -> Maybe BS.ByteString
lookupBytes ident SDLAssetManager{..} = M.lookup ident amByteMap
