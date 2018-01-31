{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Kurokos.Asset
  (
  -- ** Types
    AssetManager
  -- ** Generate
  , allocAssetManager
  , newAssetManager
  , freeAssetManager
  -- ** Get
  , lookupFont
  , lookupTexture
  , lookupBytes
  ) where

import           Control.Concurrent.MVar
import qualified Control.Exception            as E
import           Control.Monad                (foldM)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import           Data.Char                    (toLower)
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import           System.FilePath.Posix

import           Kurokos.Asset.Internal.Types (RawAssetManager (..))
import           Kurokos.Asset.Raw            (Ident)
import qualified Kurokos.Asset.Raw            as Asset
import qualified Kurokos.Graphics             as G
import qualified Kurokos.Graphics.Font        as Font

data AssetManager = AssetManager
  { amByteMap :: M.Map Ident BS.ByteString
  , amFontMap :: M.Map Ident Font.Font
  , amTexMap  :: M.Map Ident G.Texture
  }

allocAssetManager :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) => G.Renderer -> RawAssetManager -> ResourceT m AssetManager
allocAssetManager rndr (RawAssetManager bmap) =
  foldM work empty $ M.toList bmap
  where
    ft = G.getFreeType rndr
    empty = AssetManager M.empty M.empty M.empty

    work am@AssetManager{..} (ident, (path, bytes)) = update
      where
        ext = filter (/= '.') . map toLower . takeExtension $ path
        update
          | ext == "ttf" = do -- TODO: Add extensions
              font <- snd <$> allocate (Font.newFaceBS ft bytes) Font.doneFace
              return $ am {amFontMap = M.insert ident font amFontMap}
          | S.member ext imageExts = do
              tex <- snd <$> allocate (G.decodeTexture bytes) G.deleteTexture
              return $ am {amTexMap = M.insert ident tex amTexMap}
          | otherwise =
              return $ am {amByteMap = M.insert ident bytes amByteMap}

    imageExts :: S.Set String
    imageExts = S.fromList ["png", "jpg", "jpeg", "gif", "pic", "hdr", "tga", "tiff"]

-- TODO: Remove this. Font is released unexpectedly.
newAssetManager :: MonadIO m => G.Renderer -> RawAssetManager -> m AssetManager
newAssetManager rndr (RawAssetManager bmap) = liftIO $
  foldM work empty $ M.toList bmap
  where
    ft = G.getFreeType rndr
    empty = AssetManager M.empty M.empty M.empty

    work am@AssetManager{..} (ident, (path, bytes)) = update
      where
        ext = filter (/= '.') . map toLower . takeExtension $ path
        update
          | ext == "ttf" = do -- TODO: Add extensions
              font <- Font.newFaceBS ft bytes
              return $ am {amFontMap = M.insert ident font amFontMap}
          | S.member ext imageExts = do
              tex <- G.decodeTexture bytes
              return $ am {amTexMap = M.insert ident tex amTexMap}
          | otherwise =
              return $ am {amByteMap = M.insert ident bytes amByteMap}

    imageExts :: S.Set String
    imageExts = S.fromList ["png", "jpg", "jpeg", "gif", "pic", "hdr", "tga", "tiff"]

-- TODO: Remove this.
freeAssetManager :: MonadIO m => AssetManager -> m ()
freeAssetManager AssetManager{..} = liftIO $ do
  mapM_ G.deleteTexture $ M.elems amTexMap
  mapM_ Font.doneFace $ M.elems amFontMap

lookupFont :: Ident -> AssetManager -> Maybe Font.Font
lookupFont ident AssetManager{..} = M.lookup ident amFontMap

lookupTexture :: Ident -> AssetManager -> Maybe G.Texture
lookupTexture ident AssetManager{..} = M.lookup ident amTexMap

lookupBytes :: Ident -> AssetManager -> Maybe BS.ByteString
lookupBytes ident AssetManager{..} = M.lookup ident amByteMap
