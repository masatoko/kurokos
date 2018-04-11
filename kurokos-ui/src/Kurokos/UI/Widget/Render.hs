{-# LANGUAGE RecordWildCards #-}
module Kurokos.UI.Widget.Render where

import qualified Control.Exception     as E
import           Control.Lens
import           Control.Monad         (forM_)
import           Control.Monad.Extra   (whenJust)
import           Data.List.Extra       (firstJust)
import qualified Data.Text             as T
import           Linear.V3
import           Safe                  (atMay)

import           SDL                   (($=))
import qualified SDL

import qualified Kurokos.Asset         as Asset
import           Kurokos.Graphics      (ctAdvanceX, ctColor)
import qualified Kurokos.Graphics      as G
import qualified Kurokos.Graphics.Font as Font
import           Kurokos.UI.Color
import           Kurokos.UI.Def        (Renderable (..))
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Util
import           Kurokos.UI.Widget

renderWidget :: G.Renderer -> Bool -> V2 Int -> V2 Int -> WContext -> Style -> CommonResource -> Widget -> IO ()
renderWidget _r _focus _pos _parentSize _ctx _ _ Transparent = return ()

renderWidget r _focus pos _parentSize _ctx style cmnrsc Fill =
  renderBackAndBorder r pos style cmnrsc

renderWidget r _focus pos parentSize _ctx style cmnrsc Label{} = do
  renderBackAndBorder r pos style cmnrsc
  whenJust (cmnrscTextTex cmnrsc) $ renderTex_ r pos parentSize style

renderWidget r _focus pos parentSize _ctx style CmnRsc{..} (ImageView image mArea) = do
  let size = fromIntegral <$> parentSize
      rctx = G.RContext pos size Nothing Nothing
  G.renderTexture r image mArea rctx

renderWidget r _focus pos parentSize _ctx style cmnrsc Button{} = do
  renderBackAndBorder r pos style cmnrsc
  whenJust (cmnrscTextTex cmnrsc) $ renderTex_ r pos parentSize style

renderWidget r _focus pos parentSize _ctx style cmnrsc (Switch _ selected) = do
  renderBackAndBorder r pos style' cmnrsc
  whenJust (cmnrscTextTex cmnrsc) $ renderTex_ r pos parentSize style
  where
    style'
      | selected  = style&styleBgColor .~ (style^.styleTintColor)
      | otherwise = style

renderWidget r _focus pos parentSize _ctx style cmnrsc (Slider _ mKnob value) = do
  renderBackAndBorder r pos style cmnrsc
  whenJust mKnob $ \(SliderResource knobPrim valText) -> do
    renderKnob r knobPos style knobPrim
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

renderWidget r focus pos parentSize _ctx style cmnrsc (TextField _ mRsc) = do
  renderBackAndBorder r pos style cmnrsc
  whenJust mRsc $ \tfr -> do
    let height = fromMaybe fontSize $ firstJust id
                    [ view _y . G.texSize <$> txtFldRscLeft tfr
                    , view _y . G.texSize <$> txtFldRscRight tfr]
        pos' = pos & _y +~ (((parentSize^._y) - height) `div` 2) -- Centering vertically
    -- * Left text
    posCur <- case txtFldRscLeft tfr of
      Nothing  -> return pos'
      Just tex -> do
        let size = G.texSize tex
            rctx = G.RContext pos' size Nothing Nothing
        G.renderTexture r tex Nothing rctx
        return $ pos' & _x +~ (size^._x)
    -- * Cursor
    posR <- if focus
              then do
                G.setPrimColor r $ style^.styleTintColor
                G.drawPrim r (posCur&_x +~ 2) $ txtFldRscCursor tfr
                return $ posCur & _x +~ 6
              else return posCur
    -- * Right text
    whenJust (txtFldRscRight tfr) $ \tex -> do
      let size = G.texSize tex
          rctx = G.RContext posR size Nothing Nothing
      G.renderTexture r tex Nothing rctx
  where
    fontSize = style^.styleFontSize

renderWidget r focus pos (V2 width height) ctx style cmnrsc (Picker _ idx ts)
  | focus =
      forM_ (zip [0..] ts) $ \(i, tex) -> do
        let size = G.texSize tex
            pos' = pos & _y +~ (i * height)
            rctx = G.RContext pos' size Nothing Nothing
        let style' = if i == idx then ctxstHover ctxst else ctxstNormal ctxst
        renderBackAndBorder r pos' style' cmnrsc
        G.renderTexture r tex Nothing rctx
  | otherwise =
      case ts `atMay` idx of
        Just tex -> do
          renderBackAndBorder r pos style cmnrsc
          let size = G.texSize tex
              rctx = G.RContext pos size Nothing Nothing
          G.renderTexture r tex Nothing rctx
        _ -> return ()
  where
    ctxst = ctx^.ctxContextStyle

renderWidget r _focus pos parentSize _ctx style CmnRsc{..} (UserWidget a) =
  renderW r pos parentSize style a

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

genTitle :: Asset.AssetManager -> G.Renderer -> Style ->  Widget -> IO (Maybe G.Texture)
genTitle ast r style (Label title) =
  Just <$> genTextTexture ast r style title
genTitle ast r style (Button title) =
  Just <$> genTextTexture ast r style title
genTitle ast r style (Switch title _) =
  Just <$> genTextTexture ast r style title
genTitle ast r style (Slider title _ _) =
  Just <$> genTextTexture ast r style title
genTitle _ _ _ TextField{} = return Nothing
genTitle _ _ _ Picker{} = return Nothing
genTitle _ _ _ Transparent{} = return Nothing
genTitle _ _ _ Fill{} = return Nothing
genTitle _ _ _ ImageView{} = return Nothing

genTextTexture :: Asset.AssetManager -> G.Renderer -> Style -> T.Text -> IO G.Texture
genTextTexture ast rndr style title = do
  (font, size) <- getFontSize ast style
  text <- G.createTextTexture font size color title
  tex <- G.genTextImage rndr text
  G.deleteTextTexture text
  return tex
  where
    color = style^.styleTextColor

-- Internal
renderBackAndBorder :: MonadIO m => G.Renderer -> V2 Int -> Style -> CommonResource -> m ()
renderBackAndBorder r pos Style{..} CmnRsc{..} = liftIO $ do
  G.setPrimColor r _styleBgColor
  G.drawPrim r pos' cmnrscRectFill
  G.setPrimColor r _styleBorderColor
  G.drawPrim r pos' cmnrscRectBorder
  where
    pos' = fromIntegral <$> pos

renderKnob :: MonadIO m => G.Renderer -> V2 Int -> Style -> G.Prim -> m ()
renderKnob r pos Style{..} rect = liftIO $ do
  G.setPrimColor r _styleTintColor
  G.drawPrim r pos' rect
  where
    pos' = fromIntegral <$> pos

getFontSize :: MonadIO m => Asset.AssetManager -> Style -> m (Font.Font, G.FontSize)
getFontSize ast style =
  case Asset.lookupFont ident ast of
    Nothing   -> liftIO $ E.throwIO $ userError $ "Missing font: " ++ T.unpack ident
    Just font -> return (font, size)
  where
    ident = style^.styleFontIdent
    size = style^.styleFontSize
