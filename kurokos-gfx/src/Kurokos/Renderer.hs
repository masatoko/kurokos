{-# LANGUAGE RecordWildCards #-}
module Kurokos.Renderer
  ( Renderer (rndrPrimShader)
  , getFreeType
  , newRenderer
  , freeRenderer
  --
  , renderTexture
  , renderText
  ) where

import           Foreign.C.Types                              (CInt)
import           Graphics.Rendering.FreeType.Internal.Library (FT_Library)

-- import qualified Graphics.GLUtil                              as GLU
import qualified Graphics.Rendering.OpenGL                    as GL

import qualified Kurokos.Graphics.Camera                      as Cam
import           Kurokos.Graphics.Font                        (doneFreeType,
                                                               initFreeType)
import qualified Kurokos.Graphics.Render                      as Render
import           Kurokos.Graphics.Shader                      (TextureShader (..),
                                                               setProjection,
                                                               setTexture)
import qualified Kurokos.Graphics.Shader.Basic                as Basic
import qualified Kurokos.Graphics.Shader.Primitive            as Prim
import qualified Kurokos.Graphics.Shader.Text                 as Text
import qualified Kurokos.Graphics.Texture                     as Texture
import           Kurokos.Graphics.Types                       (CharTexture, ProjectionType (..),
                                                               RContext (..),
                                                               Texture (..))
import           Kurokos.Graphics.Vect

data Renderer = Renderer
  { rndrBasicShader :: Basic.BasicShader
  , rndrPrimShader  :: Prim.PrimitiveShader
  , rndrTextShader  :: Text.TextShader
  , rndrFreeType    :: FT_Library
  }

getFreeType :: Renderer -> FT_Library
getFreeType = rndrFreeType

newRenderer :: V2 CInt -> IO Renderer
newRenderer winSize = do
  b <- Basic.newBasicShader
  setProjection b Ortho winSize' True
  p <- Prim.newPrimitiveShader
  setProjection p Ortho winSize' True
  t <- Text.newTextShader
  setProjection t Ortho winSize' True
  ft <- initFreeType
  return $ Renderer b p t ft
  where
    winSize' = fromIntegral <$> winSize

freeRenderer :: Renderer -> IO ()
freeRenderer Renderer{..} =
  doneFreeType rndrFreeType
  -- TODO: Implement others

-- | Render Texture with
renderTexture :: Maybe (Point V2 Int, V2 Int) -> Renderer -> Texture -> RContext -> IO ()
renderTexture mTexPos Renderer{..} tex rctx =
  case mTexPos of
    Nothing -> work $ shdrTexCoordVbo rndrBasicShader
    Just (texCoord, texSize) -> do
      buf <- Texture.newTexCoordVbo tex texCoord texSize
      work buf
      GL.deleteObjectName buf
  where
    work texCoordVbo = do
      Basic.setTexCoordVbo rndrBasicShader texCoordVbo
      setTexture rndrBasicShader $ texObject tex
      let mv = Render.mkModelViewForNormalized Cam.camForVertFlip rctx
      Render.setModelView rndrBasicShader mv
      Render.renderTextureShader rndrBasicShader

-- | Render CharTexture list.
renderText :: Foldable t => Renderer -> V2 Int -> t CharTexture -> IO ()
renderText Renderer{..} =
  Render.renderTextTexture rndrTextShader
