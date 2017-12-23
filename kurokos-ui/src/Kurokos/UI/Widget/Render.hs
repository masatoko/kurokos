{-# LANGUAGE RecordWildCards #-}
module Kurokos.UI.Widget.Render where

import qualified Control.Exception     as E
import           Control.Lens
import           Control.Monad.Extra   (whenJust)
import qualified Data.Text             as T
import           Linear.V3

import           SDL                   (($=))
import qualified SDL

import           Kurokos.Graphics      (charTexColor)
import qualified Kurokos.Graphics      as G
import qualified Kurokos.Graphics.Font as Font
import           Kurokos.UI.Color
import           Kurokos.UI.Def        (Renderable (..))
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Util
import           Kurokos.UI.Widget

renderWidget :: G.Renderer -> V2 Int -> V2 Int -> WidgetColor -> Widget -> IO ()
renderWidget _r _pos _parentSize _ Transparent = return ()

renderWidget r _pos _parentSize WidgetColor{..} Fill = -- do
  -- clearBy r _wcBack -- TODO:
  return ()

renderWidget r pos parentSize wc@WidgetColor{..} (Label title) = do
  renderBackAndBorder r pos parentSize wc
  let title' = map (set charTexColor (wc^.wcTitle)) title -- TODO: Should not do this every frame!
  G.renderText r pos title'

renderWidget r pos parentSize WidgetColor{..} (ImageView image) = do
  let size = fromIntegral <$> parentSize
      rctx = G.RContext pos size Nothing Nothing
  G.renderTexture r image rctx

renderWidget r pos parentSize wc@WidgetColor{..} (Button title) = do
  renderBackAndBorder r pos parentSize wc
  let title' = map (set charTexColor (wc^.wcTitle)) title -- TODO: Should not do this every frame!
  G.renderText r pos title'

renderWidget r pos parentSize wcol (UserWidget a) =
  renderW r pos parentSize wcol a

-- Internal

renderBackAndBorder :: MonadIO m => G.Renderer -> V2 Int -> V2 Int -> WidgetColor -> m ()
renderBackAndBorder r pos parentSize wc@WidgetColor{..} = liftIO $ do
  back <- G.newFillRectangle r size' -- TODO: Must not new every frame! Move another place.
  rect <- G.newRectangle r size'
  --
  G.setPrimColor r _wcBack
  G.drawPrim r pos' back
  --
  G.setPrimColor r _wcBorder
  G.drawPrim r pos' rect
  --
  G.freePrim back
  G.freePrim rect
  where
    size' = fromIntegral <$> parentSize
    pos' = fromIntegral <$> pos
