module Kurokos.Render
  ( printTest
  ) where

import qualified Control.Exception    as E
import           Control.Monad.Reader
import           Data.Text            (Text)
import           Data.Word            (Word8)
import           Linear.V2
import           Linear.V3

import           Kurokos.Core
import qualified Kurokos.Graphics     as G

printTest :: MonadIO m => V2 Int -> V3 Word8 -> Text -> KurokosT m ()
printTest pos (V3 r g b) text = do -- liftIO $ print text
  font <- asks envSystemFont
  withRenderer $ \rndr ->
    E.bracket
      (G.createTextTexture font (V3 r g b) text)
      G.deleteTextTexture
      (G.renderText rndr pos)
