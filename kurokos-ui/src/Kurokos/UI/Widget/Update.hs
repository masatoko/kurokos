module Kurokos.UI.Widget.Update where

import qualified Control.Exception     as E
import           Control.Lens
import           Control.Monad.Extra   (whenJust)
import           Control.Monad.State
import qualified Data.ByteString       as BS
import qualified Data.Map              as M
import           Data.Text
import qualified Data.Text             as T
import           Linear.V3
import           System.IO             (IOMode (..), hClose, openFile)

import qualified SDL

import qualified Kurokos.Asset         as Asset
import qualified Kurokos.Asset.Raw     as Asset

import qualified Kurokos.Graphics      as G
import qualified Kurokos.Graphics.Font as Font
import           Kurokos.UI.Import
import           Kurokos.UI.Widget
import           Kurokos.UI.Color (WidgetColor (..))

-- | Called when this widget needs re-layout.
--
-- Called from Core.readyRender
-- Ready something except for CommonResource
onReadyLayout :: (RenderEnv m, MonadIO m) => V2 Int -> WidgetColor -> Widget -> m Widget
onReadyLayout (V2 w h) wc (Slider title font size mPreKnob value) = do
  -- * Release
  liftIO $ whenJust mPreKnob (G.freePrim . sliderRscKnob)
  -- * Make
  knob <- withRenderer $ \r -> G.newFillRectangle r (V2 30 (fromIntegral h))
  let text = T.pack $ showValue value
  text' <- liftIO $ G.createTextTexture font size (_wcTitle wc) text
  textTex <- withRenderer $ \r -> G.genTextImage r text'
  liftIO $ G.deleteTextTexture text'
  let rsc = SliderResource knob textTex
  return $ Slider title font size (Just rsc) value
onReadyLayout _ _ w = return w


modifyOnClicked :: Point V2 CInt -- ^ Cursor position
                -> Point V2 CInt -- ^ Widget world position
                -> V2 CInt -- ^ Widget size
                -> Widget
                -> Widget
modifyOnClicked _ _ _ (Switch title font size bool) = Switch title font size (not bool)
modifyOnClicked (P (V2 curX curY)) (P (V2 wx wy)) (V2 w h) (Slider title font size mPrim value) =
  -- Calculate value by click position
  Slider title font size mPrim value'
  where
    rate = fromIntegral (curX - wx) / fromIntegral w
    value' = updateValueByRate rate value
modifyOnClicked _ _ _ w = w

modifyWhenHoverWithLHold :: Point V2 CInt -- ^ Cursor position
                          -> Point V2 CInt -- ^ Widget world position
                          -> V2 CInt -- ^ Widget size
                          -> Widget
                          -> Widget
modifyWhenHoverWithLHold (P (V2 curX curY)) (P (V2 wx wy)) (V2 w h) (Slider title font size mPrim value) =
  -- Calculate value by click position
  Slider title font size mPrim value'
  where
    rate = fromIntegral (curX - wx) / fromIntegral w
    value' = updateValueByRate rate value
modifyWhenHoverWithLHold _ _ _ w = w
