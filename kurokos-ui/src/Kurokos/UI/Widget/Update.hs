module Kurokos.UI.Widget.Update where

import           Debug.Trace           (trace)

import qualified Control.Exception     as E
import           Control.Lens
import           Control.Monad         (unless)
import           Control.Monad.Extra   (whenJust)
import           Control.Monad.State
import qualified Data.ByteString       as BS
import           Data.List             (foldl')
import qualified Data.Map              as M
import qualified Data.Text             as T
import qualified Data.Text.Zipper      as TZ
import           Linear.V3
import           System.IO             (IOMode (..), hClose, openFile)

import qualified SDL

import qualified Kurokos.Asset         as Asset
import qualified Kurokos.Asset.Raw     as Asset

import qualified Kurokos.Graphics      as G
import qualified Kurokos.Graphics.Font as Font
import           Kurokos.UI.Types
import           Kurokos.UI.Import
import           Kurokos.UI.Widget

-- | Called when this widget needs re-layout.
--
-- Called from Core.readyRender
-- Ready something except for CommonResource
onReadyLayout :: (RenderEnv m, MonadIO m) => Asset.AssetManager -> V2 Int -> Style -> Widget -> m Widget
onReadyLayout ast (V2 w h) style (Slider title mPreKnob value) = do
  (font, size) <- getFontSize ast style
  -- * Release
  liftIO $ whenJust mPreKnob $ \rsc -> do
    G.freePrim $ sliderRscKnob rsc
    G.deleteTexture $ sliderRscText rsc
  -- * Make
  knob <- withRenderer $ \r -> G.newFillRectangle r (V2 30 (fromIntegral h))
  textTex <- genTextTexture font size (_styleTextColor style) (T.pack $ showValue value)
  let rsc = SliderResource knob textTex
  return $ Slider title (Just rsc) value
onReadyLayout ast (V2 w h) style (TextField z mRsc) = do
  (font, size) <- getFontSize ast style
  -- * Release
  liftIO $ whenJust mRsc $ \rsc -> do
    whenJust (txtFldRscLeft rsc) G.deleteTexture
    whenJust (txtFldRscRight rsc) G.deleteTexture
  -- * Make
  cursor <- withRenderer $ \r -> G.newFillRectangle r (V2 2 (fromIntegral size))
  --
  let (textL, textR) = T.splitAt row $ TZ.currentLine z
        where (_,row) = TZ.cursorPosition z
  mTexL <- if T.null textL
            then return Nothing
            else Just <$> genTextTexture font size (_styleTextColor style) textL
  mTexR <- if T.null textR
            then return Nothing
            else Just <$> genTextTexture font size (_styleTextColor style) textR
  let rsc = TextFieldResource cursor mTexL mTexR
  return $ TextField z (Just rsc)
onReadyLayout ast (V2 w h) style (Picker ts idx textures0) = do
  (font, size) <- getFontSize ast style
  -- * Release
  unless (null textures0) $ liftIO $ mapM_ G.deleteTexture textures0
  -- * Make
  textures <- mapM (genTextTexture font size (_styleTextColor style) . snd) ts
  return $ Picker ts idx textures
onReadyLayout _ _ _ w = return w

genTextTexture :: (RenderEnv m, MonadIO m) => Font.Font -> G.FontSize -> G.Color -> T.Text -> m G.Texture
genTextTexture font size color text = do
  text' <- liftIO $ G.createTextTexture font size color text
  textTex <- withRenderer $ \r -> G.genTextImage r text'
  liftIO $ G.deleteTextTexture text'
  return textTex

modifyOnClicked :: WContext
                -> Point V2 CInt -- ^ Cursor position
                -> Point V2 CInt -- ^ Widget world position
                -> V2 CInt -- ^ Widget size
                -> Widget
                -> Widget
modifyOnClicked _ _ _ _ (Switch title bool) = Switch title (not bool)
modifyOnClicked _ (P (V2 curX curY)) (P (V2 wx wy)) (V2 w h) (Slider title mPrim value) =
  -- Calculate value by click position
  Slider title mPrim value'
  where
    rate = fromIntegral (curX - wx) / fromIntegral w
    value' = updateValueByRate rate value
modifyOnClicked ctx (P (V2 curX curY)) (P (V2 wx wy)) _ w0@(Picker ts _ textures)
  | focus     = Picker ts idx textures
  | otherwise = w0
  where
    V2 w h = wstSize $ ctx^.ctxWidgetState
    idx = fromIntegral $ (curY - wy) `div` h
    focus = ctx^.ctxWidgetState.wstFocus
modifyOnClicked _ _ _ _ w = w

modifyWhenHoverWithLHold :: Point V2 CInt -- ^ Cursor position
                          -> Point V2 CInt -- ^ Widget world position
                          -> V2 CInt -- ^ Widget size
                          -> Widget
                          -> Maybe Widget
modifyWhenHoverWithLHold (P (V2 curX curY)) (P (V2 wx wy)) (V2 w h) (Slider title mPrim value) =
  -- Calculate value by click position
  Just $ Slider title mPrim value'
  where
    rate = fromIntegral (curX - wx) / fromIntegral w
    value' = updateValueByRate rate value
modifyWhenHoverWithLHold _ _ _ w = Nothing

getFontSize :: MonadIO m => Asset.AssetManager -> Style -> m (Font.Font, G.FontSize)
getFontSize ast style =
  case Asset.lookupFont ident ast of
    Nothing   -> liftIO $ E.throwIO $ userError $ "Missing font: " ++ T.unpack ident
    Just font -> return (font, size)
  where
    ident = style^.styleFontIdent
    size = style^.styleFontSize
