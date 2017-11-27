module Kurokos.Font
  ( withFont
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

withFont :: FontSource -> Int -> (Font -> IO a) -> IO a
withFont src size action =
  case src of
    FontFile path -> E.bracket (Font.load path size) Font.free action
    FontBinary bs -> E.bracket (Font.decode bs size) Font.free action
