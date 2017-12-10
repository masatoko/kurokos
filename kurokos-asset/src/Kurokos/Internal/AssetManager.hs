{-# LANGUAGE RecordWildCards #-}
module Kurokos.Internal.AssetManager where

import           Control.Monad          (foldM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString        as BS
import           Data.Char              (toLower)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import qualified Data.Set               as S
import qualified Data.Text              as T
import           System.FilePath.Posix

import qualified SDL
import qualified SDL.Font               as Font
import qualified SDL.Image              as Image

import           Kurokos.Internal.Types

loadAssetManager :: MonadIO m => SDL.Renderer -> AssetFile -> m AssetManager
loadAssetManager r (AssetFile as) =
  liftIO $ foldM work empty as
  where
    empty = AssetManager M.empty M.empty M.empty

    work am@AssetManager{..} AssetInfo{..} = update
      where
        path =
          case aiDirectory of
            Nothing  -> aiFileName
            Just dir -> dir </> aiFileName
        ext = filter (/= '.') . map toLower . takeExtension $ aiFileName
        ident' = fromMaybe (T.pack aiFileName) aiIdent
        update
          | ext == "ttf" = do
              font <- Font.load path (fromMaybe 16 aiSize)
              return $ am {fontMap = M.insert ident' font fontMap}
          | ext == "tga" = do
              tex <- Image.loadTextureTGA r path
              return $ am {textureMap = M.insert ident' tex textureMap}
          | S.member ext imageEtxs = do
              tex <- Image.loadTexture r path
              return $ am {textureMap = M.insert ident' tex textureMap}
          | otherwise    = do
              bytes <- BS.readFile path
              return $ am {byteMap = M.insert ident' bytes byteMap}

imageEtxs :: S.Set String
imageEtxs = S.fromList ["bmp", "gif", "jpeg", "lbm", "pcx", "png", "pnm", "svg", "tiff", "webp", "xcf", "xpm", "xv"]

lookupFont :: Ident -> AssetManager -> Maybe Font.Font
lookupFont ident AssetManager{..} = M.lookup ident fontMap

lookupTexture :: Ident -> AssetManager -> Maybe SDL.Texture
lookupTexture ident AssetManager{..} = M.lookup ident textureMap

lookupBytes :: Ident -> AssetManager -> Maybe BS.ByteString
lookupBytes ident AssetManager{..} = M.lookup ident byteMap
