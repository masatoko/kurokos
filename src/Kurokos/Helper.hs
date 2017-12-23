module Kurokos.Helper
  ( printTest
  ) where

import qualified Control.Exception    as E
import           Control.Monad.Reader (MonadIO, asks)
import           Data.Text            (Text)
import           Data.Word            (Word8)
import           Linear.V2
import           Linear.V4

import           Kurokos.Core
import qualified Kurokos.Graphics     as G

printTest :: MonadIO m => V2 Int -> V4 Word8 -> Text -> KurokosT m ()
printTest pos color text = do
  font <- asks envSystemFont
  withRenderer $ \rndr ->
    E.bracket
      (G.createTextTexture font 18 color text)
      G.deleteTextTexture
      (G.renderText rndr pos)
