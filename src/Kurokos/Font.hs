module Kurokos.Font
  ( loadFont
  , freeFont
  , withFont
  , withFontB
  ) where

import qualified Control.Exception.Safe   as E
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import           Data.ByteString.Internal (ByteString (..))
import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Ptr              (plusPtr)
import           System.Directory         (doesFileExist)

import qualified SDL.Font                 as Font

import           Kurokos.Types             (Font)

loadFont :: MonadIO m => FilePath -> Int -> m Font
loadFont path size = liftIO $ do
  p <- doesFileExist path
  if p
    then Font.load path size
    else E.throwIO $ userError $ "Missing font file: " ++ path

freeFont :: MonadIO m => Font -> m ()
freeFont font =
  liftIO $ Font.free font

withFont :: FilePath -> Int -> (Font -> IO a) -> IO a
withFont path size action =
  E.bracket (Font.load path size)
            Font.free
            action

withFontB :: ByteString -> Int -> (Font -> IO a) -> IO a
withFontB bs size action =
  E.bracket (Font.decode bs size)
            Font.free
            action
