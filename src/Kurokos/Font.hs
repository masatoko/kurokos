module Kurokos.Font
  ( loadFont
  , freeFont
  , withFont
  ) where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import qualified Control.Exception.Safe   as E
import qualified Data.ByteString          as B
import           Data.ByteString          (ByteString)
import           Data.ByteString.Internal (ByteString (..))
import           System.Directory         (doesFileExist)
import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Ptr              (plusPtr)

import qualified SDL.Font                 as Font

import           Kurokos.Data            (Font, Sprite (..))

loadFont :: MonadIO m => FilePath -> Int -> m Font
loadFont path size = liftIO $ do
  p <- doesFileExist path
  if p
    then Font.load path size
    else E.throwIO $ userError $ "Missing font file: " ++ path

freeFont :: MonadIO m => Font -> m ()
freeFont font =
  liftIO $ Font.free font

withFont :: ByteString -> Int -> (Font -> IO a) -> IO a
withFont bs size action =
  E.bracket (Font.decode bs size)
            Font.free
            action