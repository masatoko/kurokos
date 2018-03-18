{-# LANGUAGE RecordWildCards #-}
module Kurokos.UI.Widget.Render where

import qualified Control.Exception     as E
import           Control.Lens
import           Control.Monad.Extra   (whenJust)
import qualified Data.Text             as T
import           Linear.V3

import           SDL                   (($=))
import qualified SDL

import           Kurokos.Graphics      (ctColor, ctAdvanceX)
import qualified Kurokos.Graphics      as G
import qualified Kurokos.Graphics.Font as Font
import           Kurokos.UI.Color
import           Kurokos.UI.Def        (Renderable (..))
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Util
import           Kurokos.UI.Widget

renderWidget :: G.Renderer -> V2 Int -> V2 Int -> WidgetColor -> Style -> CommonResource -> Widget -> IO ()
renderWidget _r _pos _parentSize _ _ _ Transparent = return ()

renderWidget r pos _parentSize wc style cmnrsc Fill = do
  renderBackAndBorder r pos wc cmnrsc

renderWidget r pos parentSize wc@WidgetColor{..} style cmnrsc Label{} = do
  renderBackAndBorder r pos wc cmnrsc
  whenJust (cmnrscTextTex cmnrsc) $ renderTex_ r pos parentSize style

renderWidget r pos parentSize WidgetColor{..} style CmnRsc{..} (ImageView image) = do
  let size = fromIntegral <$> parentSize
      rctx = G.RContext pos size Nothing Nothing
  G.renderTexture r image Nothing rctx

renderWidget r pos parentSize wc@WidgetColor{..} style cmnrsc Button{} = do
  renderBackAndBorder r pos wc cmnrsc
  whenJust (cmnrscTextTex cmnrsc) $ renderTex_ r pos parentSize style

renderWidget r pos parentSize wc style cmnrsc (Switch _ _ _ selected) = do
  renderBackAndBorder r pos wc' cmnrsc
  whenJust (cmnrscTextTex cmnrsc) $ renderTex_ r pos parentSize style
  where
    wc'
      | selected  = wc&wcBack .~ (wc^.wcTint)
      | otherwise = wc

renderWidget r pos parentSize wc@WidgetColor{..} style cmnrsc (Slider _ _ _ mKnob value) = do
  renderBackAndBorder r pos wc cmnrsc
  whenJust mKnob $ \(SliderResource knobPrim valText) -> do
    renderKnob r knobPos wc knobPrim
    renderValue valText
    whenJust (cmnrscTextTex cmnrsc) renderTitle
  where
    renderValue tex = G.renderTexture r tex Nothing rctx
      where
        rctx = G.RContext pos' size Nothing Nothing
        size = G.texSize tex
        pos' = pos & _x +~ (parWidth - texWidth - 5)
          where
            parWidth = parentSize^._x
            texWidth = size^._x
    renderTitle tex = G.renderTexture r tex Nothing rctx
      where
        rctx = G.RContext pos' (G.texSize tex) Nothing Nothing
        pos' = pos & _x +~ 5
    knobPos = pos & _x +~ dx
      where
        dx = round $ fromIntegral (parentSize^._x - 30) * rateFromValue value

renderWidget r pos parentSize wcol style CmnRsc{..} (UserWidget a) =
  renderW r pos parentSize wcol a

renderTex_ :: G.Renderer -> V2 Int -> V2 Int -> Style -> G.Texture -> IO ()
renderTex_ r pos parentSize style tex =
  G.renderTexture r tex Nothing rctx
  where
    rctx = G.RContext pos' size Nothing Nothing
    size = G.texSize tex

    dsize = parentSize - size
    dy = (dsize^._y) `div` 2
    pos' = mkPos $ style^.styleTextAlign
      where
        mkPos TALeft   = pos & _y +~ dy
        mkPos TARight  = pos & _x +~ dx & _y +~ dy
          where dx = dsize^._x
        mkPos TACenter = pos & _x +~ dx & _y +~ dy
          where dx = (dsize^._x) `div` 2

genTitle :: G.Renderer -> WidgetColor ->  Widget -> IO (Maybe G.Texture)
genTitle r wc (Label title font size) =
  Just <$> genTitle_ r font size (wc^.wcTitle) title
genTitle r wc (Button title font size) =
  Just <$> genTitle_ r font size (wc^.wcTitle) title
genTitle r wc (Switch title font size _) =
  Just <$> genTitle_ r font size (wc^.wcTitle) title
genTitle r wc (Slider title font size _ _) =
  Just <$> genTitle_ r font size (wc^.wcTitle) title
genTitle _ _ Transparent{} = return Nothing
genTitle _ _ Fill{} = return Nothing
genTitle _ _ ImageView{} = return Nothing

genTitle_ :: G.Renderer -> Font.Font -> G.FontSize -> G.Color -> T.Text -> IO G.Texture
genTitle_ rndr font size color title = do
  text <- G.createTextTexture font size color title
  tex <- G.genTextImage rndr text
  G.deleteTextTexture text
  return tex

-- Internal
renderBackAndBorder :: MonadIO m => G.Renderer -> V2 Int -> WidgetColor -> CommonResource -> m ()
renderBackAndBorder r pos wc@WidgetColor{..} CmnRsc{..} = liftIO $ do
  G.setPrimColor r _wcBack
  G.drawPrim r pos' cmnrscRectFill
  G.setPrimColor r _wcBorder
  G.drawPrim r pos' cmnrscRectBorder
  where
    pos' = fromIntegral <$> pos

renderKnob :: MonadIO m => G.Renderer -> V2 Int -> WidgetColor -> G.Prim -> m ()
renderKnob r pos WidgetColor{..} rect = liftIO $ do
  G.setPrimColor r _wcTint
  G.drawPrim r pos' rect
  where
    pos' = fromIntegral <$> pos
