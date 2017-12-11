{-# LANGUAGE FlexibleContexts #-}
module Kurokos.Render
  ( setColor
  , clearBy
  , printTest
  ) where

import           Control.Exception.Safe (MonadMask)
import qualified Control.Exception.Safe as E
import           Control.Monad.Managed  (managed, runManaged)
import           Control.Monad.Reader
import           Data.Text              (Text)
import           Data.Word              (Word8)
import           Linear.Affine          (Point (..))
import           Linear.V2
import           Linear.V4

import           SDL                    (($=))
import qualified SDL
import qualified SDL.Font               as Font

import           Kurokos.Core

setColor :: (MonadIO m, MonadMask m) => V4 Word8 -> KurokosT m ()
setColor color =
  withRenderer $ \r ->
    SDL.rendererDrawColor r $= color

clearBy :: (MonadIO m, MonadMask m) => V4 Word8 -> KurokosT m ()
clearBy color =
  withRenderer $ \r -> do
    SDL.rendererDrawColor r $= color
    SDL.clear r

printTest :: (MonadIO m, MonadMask m) => Point V2 Int -> V4 Word8 -> Text -> KurokosT m ()
printTest pos color text = do
  font <- asks systemFont
  withRenderer $ \r -> liftIO $ do
    (w,h) <- Font.size font text
    runManaged $ do
      surface <- managed $ E.bracket (Font.blended font color text) SDL.freeSurface
      texture <- managed $ E.bracket (SDL.createTextureFromSurface r surface) SDL.destroyTexture
      let rect = Just $ SDL.Rectangle pos' (fromIntegral <$> V2 w h)
      SDL.copy r texture Nothing rect
  where
    pos' = fromIntegral <$> pos
