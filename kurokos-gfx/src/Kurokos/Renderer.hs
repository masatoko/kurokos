{-# LANGUAGE RecordWildCards #-}
module Kurokos.Renderer
  ( Renderer
  , newRenderer
  --
  , renderTexture
  , renderTexture_
  , renderText
  ) where

import           Linear.V2

import qualified Kurokos.Graphics.Camera       as Cam
import           Kurokos.Graphics.Render       (renderByShader,
                                                renderTextTexture)
import           Kurokos.Graphics.Shader       (RContext, setProjection,
                                                setTexture)
import qualified Kurokos.Graphics.Shader.Basic as Basic
import qualified Kurokos.Graphics.Shader.Text  as Text
import           Kurokos.Graphics.Types        (CharTexture, Texture (..), ProjectionType (..))

data Renderer = Renderer
  { rndrBasicShader :: Basic.BasicShader
  , rndrTextShader  :: Text.TextShader
  }

newRenderer :: V2 Int -> IO Renderer
newRenderer winSize = do
  b <- Basic.newBasicShader
  setProjection Ortho winSize' b
  t <- Text.newTextShader
  setProjection Ortho winSize' t
  return $ Renderer b t
  where
    winSize' = fromIntegral <$> winSize

-- TODO: Implement freeRenderer

-- | Render Texture with camera.
renderTexture :: Renderer -> Cam.Camera -> Texture -> RContext -> IO ()
renderTexture Renderer{..} cam tex rctx = do
  setTexture rndrBasicShader $ texObject tex
  renderByShader rndrBasicShader cam rctx

-- | Render Texture with default camera.
-- Default camera is Camera.mkCameara
renderTexture_ :: Renderer -> Texture -> RContext -> IO ()
renderTexture_ rndr = renderTexture rndr Cam.mkCamera

-- | Render CharTexture list.
renderText :: Foldable t => Renderer -> V2 Int -> t CharTexture -> IO ()
renderText Renderer{..} =
  renderTextTexture rndrTextShader
