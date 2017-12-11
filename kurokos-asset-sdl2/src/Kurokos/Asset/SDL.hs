{-# LANGUAGE RecordWildCards #-}
module Kurokos.Asset.SDL where

import           Control.Monad                (foldM)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import qualified Data.ByteString              as BS
import           Data.Char                    (toLower)
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import qualified SDL
import           System.FilePath.Posix

import qualified SDL.Image                    as Image

import           Kurokos.Asset                (Ident)
import qualified Kurokos.Asset                as Asset
import           Kurokos.Asset.Internal.Types (AssetManager (..))

data SDLAssetManager = SDLAssetManager
  { byteMap    :: M.Map Ident BS.ByteString
  , fontMap    :: M.Map Ident BS.ByteString
  , textureMap :: M.Map Ident SDL.Texture
  }

allocSDL :: MonadIO m => SDL.Renderer -> AssetManager -> m SDLAssetManager
allocSDL r (AssetManager bmap) =
  foldM work empty $ M.toList bmap
  where
    empty = SDLAssetManager M.empty M.empty M.empty
    work am@SDLAssetManager{..} (ident, (path, bytes)) = update
      where
        ext = filter (/= '.') . map toLower . takeExtension $ path
        update
          | ext == "ttf" =
              return $ am {fontMap = M.insert ident bytes fontMap}
          | ext == "tga" = do
              tex <- Image.decodeTextureTGA r bytes
              return $ am {textureMap = M.insert ident tex textureMap}
          | S.member ext imageEtxs = do
              tex <- Image.decodeTexture r bytes
              return $ am {textureMap = M.insert ident tex textureMap}
          | otherwise    =
              return $ am {byteMap = M.insert ident bytes byteMap}

    imageEtxs :: S.Set String
    imageEtxs = S.fromList ["bmp", "gif", "jpeg", "lbm", "pcx", "png", "pnm", "svg", "tiff", "webp", "xcf", "xpm", "xv"]

lookupFont :: Ident -> SDLAssetManager -> Maybe BS.ByteString
lookupFont ident SDLAssetManager{..} = M.lookup ident fontMap

lookupTexture :: Ident -> SDLAssetManager -> Maybe SDL.Texture
lookupTexture ident SDLAssetManager{..} = M.lookup ident textureMap

lookupBytes :: Ident -> SDLAssetManager -> Maybe BS.ByteString
lookupBytes ident SDLAssetManager{..} = M.lookup ident byteMap

-- | Deprecated
-- loadAssetManager :: MonadIO m => SDL.Renderer -> AssetList -> m SDLAssetManager
-- loadAssetManager r (AssetList as) =
--   liftIO $ foldM work empty as
--   where
--     empty = SDLAssetManager M.empty M.empty M.empty
--
--     work am@SDLAssetManager{..} AssetInfo{..} = update
--       where
--         path =
--           case aiDirectory of
--             Nothing  -> aiFileName
--             Just dir -> dir </> aiFileName
--         ext = filter (/= '.') . map toLower . takeExtension $ aiFileName
--         ident' = fromMaybe (T.pack aiFileName) aiIdent
--         update
--           | ext == "ttf" = do
--               font <- Font.load path (fromMaybe 16 aiSize)
--               return $ am {fontMap = M.insert ident' font fontMap}
--           | ext == "tga" = do
--               tex <- Image.loadTextureTGA r path
--               return $ am {textureMap = M.insert ident' tex textureMap}
--           | S.member ext imageEtxs = do
--               tex <- Image.loadTexture r path
--               return $ am {textureMap = M.insert ident' tex textureMap}
--           | otherwise    = do
--               bytes <- BS.readFile path
--               return $ am {byteMap = M.insert ident' bytes byteMap}
