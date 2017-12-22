{-# LANGUAGE RecordWildCards #-}
module Kurokos.Renderer
  ( Renderer
  , getFreeType
  , newRenderer
  , freeRenderer
  --
  , renderTexture
  , renderTexture_
  , renderText
  ) where

import           Foreign.C.Types                              (CInt)
import           Graphics.Rendering.FreeType.Internal.Library (FT_Library)
import           Linear.V2

import qualified Kurokos.Graphics.Camera                      as Cam
import           Kurokos.Graphics.Font                        (doneFreeType,
                                                               initFreeType)
import           Kurokos.Graphics.Render                      (renderByShader,
                                                               renderTextTexture)
import           Kurokos.Graphics.Shader                      (RContext,
                                                               setProjection,
                                                               setTexture)
import qualified Kurokos.Graphics.Shader.Basic                as Basic
import qualified Kurokos.Graphics.Shader.Text                 as Text
import           Kurokos.Graphics.Types                       (CharTexture, ProjectionType (..),
                                                               Texture (..))

data Renderer = Renderer
  { rndrBasicShader :: Basic.BasicShader
  , rndrTextShader  :: Text.TextShader
  , rndrFreeType    :: FT_Library
  }

getFreeType :: Renderer -> FT_Library
getFreeType = rndrFreeType

newRenderer :: V2 CInt -> IO Renderer
newRenderer winSize = do
  b <- Basic.newBasicShader
  setProjection Ortho winSize' b
  t <- Text.newTextShader
  setProjection Ortho winSize' t
  ft <- initFreeType
  return $ Renderer b t ft
  where
    winSize' = fromIntegral <$> winSize

freeRenderer :: Renderer -> IO ()
freeRenderer Renderer{..} =
  doneFreeType rndrFreeType
  -- TODO: Implement others

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
