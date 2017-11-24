module Protonic.Font
  ( newFont
  , freeFont
  , withFont
  , ascent, descent
  , GlyphMetrics (..)
  , glyphMetrics
  ) where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import qualified Control.Exception.Safe   as E
import qualified Data.ByteString          as B
import           Data.ByteString          (ByteString)
import           Data.ByteString.Internal (ByteString (..))
import           System.Directory         (doesFileExist)
import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Ptr              (plusPtr)

import qualified SDL.TTF                  as TTF

import           Protonic.Data            (Font (..), Sprite (..))
import           Protonic.TTFHelper       (GlyphMetrics (..), rawGlyphMetrics, fontFromBytes)

newFont :: MonadIO m => FilePath -> Int -> m Font
newFont path size = liftIO $ do
  p <- doesFileExist path
  if p
    then Font <$> TTF.openFont path size
    else E.throwIO $ userError $ "Missing font file: " ++ path

freeFont :: MonadIO m => Font -> m ()
freeFont (Font font) =
  liftIO $ TTF.closeFont font

withFont :: ByteString -> Int -> (Font -> IO a) -> IO a
withFont (PS fptr off len) size action =
  withForeignPtr fptr $ \ptr ->
    E.bracket (fontFromBytes (ptr `plusPtr` off) len size)
              TTF.closeFont
              (action . Font)

ascent :: MonadIO m => Font -> m Int
ascent (Font font) =
  liftIO $ TTF.getFontAscent font

descent :: MonadIO m => Font -> m Int
descent (Font font) =
  liftIO $ TTF.getFontDescent font

glyphMetrics :: MonadIO m => Font -> Char -> m GlyphMetrics
glyphMetrics (Font font) c =
  liftIO $ rawGlyphMetrics font c
