{-# LANGUAGE RecordWildCards #-}
module Kurokos.UI.Widget.Render where

import qualified Control.Exception   as E
import           Control.Monad.Extra (whenJust)
import qualified Data.Text           as T

import           SDL                 (($=))
import qualified SDL
import qualified SDL.Font            as Font
import qualified SDL.Primitive       as Prim

import           Kurokos.UI.Color
import           Kurokos.UI.Def      (Renderable (..))
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Util
import           Kurokos.UI.Widget

renderWidget :: SDL.Renderer -> GuiSize -> WidgetColor -> Widget -> IO ()
renderWidget _r _parentSize _ Transparent = return ()

renderWidget r _parentSize WidgetColor{..} Fill =
  clearBy r _wcBack

renderWidget r parentSize wc@WidgetColor{..} (Label title font) = do
  (tex, size) <- makeTextTexture r wc font title
  renderBackAndBorder r parentSize wc
  --
  let pos = P $ (`div` 2) <$> parentSize - size
  SDL.copy r tex Nothing $ Just (Rectangle pos size)

renderWidget r _parentSize WidgetColor{..} (ImageView image) = do
  clearBy r _wcBack
  SDL.copy r image Nothing Nothing

renderWidget r parentSize wc@WidgetColor{..} (Button title font) = do
  (tex, size) <- makeTextTexture r wc font title
  renderBackAndBorder r parentSize wc
  let pos = P $ (`div` 2) <$> parentSize - size
  SDL.copy r tex Nothing $ Just (Rectangle pos size)

-- renderWidget r parentSize wcol (UserWidget a) =
--   renderW r parentSize wcol a

-- Internal

makeTextTexture :: Num b => SDL.Renderer -> WidgetColor -> Font.Font -> T.Text -> IO (SDL.Texture, V2 b)
makeTextTexture r WidgetColor{..} font text = do
  (w,h) <- Font.size font text
  let size = fromIntegral <$> V2 w h
  tex <- E.bracket
    (Font.blended font _wcTitle text)
    SDL.freeSurface
    (SDL.createTextureFromSurface r)
  return (tex, size)

renderBackAndBorder :: MonadIO m => SDL.Renderer -> V2 CInt -> WidgetColor -> m ()
renderBackAndBorder r parentSize WidgetColor{..} = do
  Prim.fillRoundRectangle r (pure 0) size' 3 _wcBack
  Prim.roundRectangle r (pure 0) size' 5 _wcBorder
  where
    size' = (+ (-1)) <$> parentSize
