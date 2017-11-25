module Kurokos.Render
  ( setColor
  , clearBy
  , drawLine
  , drawRect, fillRect
  , renderS, renderS'
  , printTest
  ) where

import           Control.Exception     (bracket)
import           Control.Monad.Managed (managed, runManaged)
import           Control.Monad.Reader
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import           Data.Word             (Word8)
import           Foreign.C.Types       (CInt)
import           Linear.Affine         (Point (..))
import           Linear.V2
import           Linear.V4

import           SDL                   (($=))
import qualified SDL
import qualified SDL.Font              as Font

import           Kurokos.Core
import           Kurokos.Data         (Sprite (..))

setColor :: V4 Word8 -> KurokosT ()
setColor color =
  withRenderer $ \r ->
    SDL.rendererDrawColor r $= color

clearBy :: V4 Word8 -> KurokosT ()
clearBy color =
  withRenderer $ \r -> do
    SDL.rendererDrawColor r $= color
    SDL.clear r

renderS :: Sprite -> Point V2 Int -> Maybe (V2 CInt) -> Maybe Double -> KurokosT ()
renderS spr pos mSize mDeg =
  renderS' spr pos mSize mDeg Nothing

renderS' :: Sprite -> Point V2 Int -> Maybe (V2 CInt) -> Maybe Double -> Maybe (Point V2 CInt) -> KurokosT ()
renderS' (Sprite tex size) pos mSize mDeg mRotCenter =
  withRenderer $ copy mDeg
  where
    pos' = fromIntegral <$> pos
    size' = fromMaybe size mSize
    dest = Just $ SDL.Rectangle pos' size'
    --
    copy (Just deg) r =
      SDL.copyEx r tex Nothing dest deg' mRotCenter (V2 False False)
      where
        deg' = realToFrac deg
    copy Nothing r = SDL.copy r tex Nothing dest

drawLine :: Point V2 Int -> Point V2 Int -> KurokosT ()
drawLine org dst =
  withRenderer $ \r ->
    SDL.drawLine r org' dst'
  where
    org' = fromIntegral <$> org
    dst' = fromIntegral <$> dst

drawRect :: Point V2 Int -> V2 Int -> KurokosT ()
drawRect p s =
  withRenderer $ \r ->
    SDL.drawRect r (Just (SDL.Rectangle p' s'))
  where
    p' = fromIntegral <$> p
    s' = fromIntegral <$> s

fillRect :: Point V2 Int -> V2 Int -> KurokosT ()
fillRect p s =
  withRenderer $ \r ->
    SDL.fillRect r (Just (SDL.Rectangle p' s'))
  where
    p' = fromIntegral <$> p
    s' = fromIntegral <$> s

printTest :: Point V2 Int -> V4 Word8 -> Text -> KurokosT ()
printTest pos color text = do
  font <- asks systemFont
  withRenderer $ \r -> do
    (w,h) <- Font.size font text
    runManaged $ do
      surface <- managed $ bracket (Font.blended font color text) SDL.freeSurface
      texture <- managed $ bracket (SDL.createTextureFromSurface r surface) SDL.destroyTexture
      let rect = Just $ SDL.Rectangle pos' (fromIntegral <$> V2 w h)
      SDL.copy r texture Nothing rect
  where
    pos' = fromIntegral <$> pos
