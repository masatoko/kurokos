{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kurokos.Asset
  ( testAssets
  -- ** Type
  , AssetManager
  , Ident
  -- ** Load
  , decodeAssetFile
  , loadAssetManager
  -- ** Find Assets
  , lookupBytes
  , lookupFont
  , lookupTexture
  ) where

import qualified Control.Exception.Safe as E
import           Control.Monad          (foldM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString        as BS
import           Data.Char              (toLower)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Data.Yaml              (FromJSON (..), (.:), (.:?))
import qualified Data.Yaml              as Y
import           System.FilePath.Posix

import qualified SDL
import qualified SDL.Font               as Font
import qualified SDL.Image              as Image

type Ident = T.Text

data AssetManager = AssetManager
  { byteMap    :: M.Map Ident BS.ByteString
  , fontMap    :: M.Map Ident Font.Font
  , textureMap :: M.Map Ident SDL.Texture
  }

data AssetInfo = AssetInfo
  { aiIdent     :: Maybe Ident
  , aiDirectory :: Maybe FilePath
  , aiFileName  :: String
  , aiSize      :: Maybe Int
  } deriving Show

newtype AssetFile = AssetFile [AssetInfo] deriving Show

testAssets :: MonadIO m => SDL.Renderer -> FilePath -> m ()
testAssets r path = liftIO $ do
  bytes <- BS.readFile path
  af <- decodeAssetFile bytes
  print af
  am <- loadAssetManager r af
  print . M.keys . byteMap $ am

decodeAssetFile :: MonadIO m => BS.ByteString -> m AssetFile
decodeAssetFile bytes = liftIO $
  case Y.decodeEither' bytes of
    Left e   -> E.throwIO e
    Right af -> return af

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

instance FromJSON AssetInfo where
  parseJSON (Y.Object v) = AssetInfo
    <$> v .:? "id"
    <*> v .:? "dir"
    <*> v .: "file"
    <*> v .:? "size"
  parseJSON _ = fail "Expected Object for AssetInfo"

instance FromJSON AssetFile where
  parseJSON (Y.Object v) = AssetFile
    <$> v .: "assets"
  parseJSON _            = fail "Expected Object for AssetFile"
