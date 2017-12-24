module Kurokos.Helper
  ( printTest
  ) where

import           Control.Monad.Reader (MonadIO, asks)
import           Data.Char            (ord)
import           Data.Maybe           (mapMaybe)
import qualified Data.Vector          as V
import           Data.Word            (Word8)
import           Linear.V2
import           Linear.V4

import           Kurokos.Core
import qualified Kurokos.Graphics     as G

printTest :: MonadIO m => V2 Int -> V4 Word8 -> String -> KurokosT m ()
printTest pos color cs = do
  ts <- asks envAsciiTextures
  let text = mapMaybe (\c -> ts V.!? ord c) cs
      -- text' = map (\cx -> G._ctColor ct .~ color) text
  withRenderer $ \r -> G.renderText r pos text
