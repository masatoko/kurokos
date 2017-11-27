module Kurokos.Font
  ( loadFont
  , freeFont
  , withFont
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

import           Kurokos.Types            (Font, FontSource (..))

loadFont :: MonadIO m => FontSource -> Int -> m Font
loadFont src size =
  case src of
    FontFile path -> Font.load path size
    FontBinary bs -> Font.decode bs size

freeFont :: MonadIO m => Font -> m ()
freeFont = Font.free

withFont :: FontSource -> Int -> (Font -> IO a) -> IO a
withFont src size = E.bracket (loadFont src size) freeFont
