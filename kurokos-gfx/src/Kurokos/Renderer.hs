{-# LANGUAGE RecordWildCards #-}
module Kurokos.Renderer
  ( Renderer (rndrPrimShader, rndrCurrentView)
  , getFreeType
  , newRenderer
  , freeRenderer
  , withProjView
  , clearRenderArea
  , setRenderArea
  , withRenderArea
  --
  , renderTexture
  , renderText
  , genTextImage
  ) where

import           Foreign.C.Types                              (CInt)
import           Graphics.Rendering.FreeType.Internal.Library (FT_Library)
import           Linear.Matrix                                (M44, (!*!))

-- import qualified Graphics.GLUtil                              as GLU
import qualified Graphics.Rendering.OpenGL                    as GL

import qualified Kurokos.Graphics.Camera                      as Cam
import           Kurokos.Graphics.Font                        (doneFreeType,
                                                               initFreeType)
import           Kurokos.Graphics.Matrix                      (mkOrtho)
import qualified Kurokos.Graphics.Render                      as Render
import           Kurokos.Graphics.Shader                      (TextureShader (..),
                                                               setProjection,
                                                               setTexture)
import qualified Kurokos.Graphics.Shader.Basic                as Basic
import qualified Kurokos.Graphics.Shader.Primitive            as Prim
import qualified Kurokos.Graphics.Shader.Text                 as Text
import qualified Kurokos.Graphics.Texture                     as Texture
import           Kurokos.Graphics.Types                       (CharTexture,
                                                               RContext (..),
                                                               TagVec2,
                                                               Texture (..),
                                                               TypedBufferObject (..))
import           Kurokos.Graphics.Vect

data Renderer = Renderer
  { rndrBasicShader :: Basic.BasicShader
  , rndrPrimShader  :: Prim.PrimitiveShader
  , rndrTextShader  :: Text.TextShader
  , rndrFreeType    :: FT_Library
  -- Env
  , rndrBasisProj   :: M44 Float -- ^ Basis orthographic projection matrix. (Don't change)
  -- State
  , rndrCurrentView :: M44 Float -- ^ Current viewing matrix. (Just a state)
  }

getFreeType :: Renderer -> FT_Library
getFreeType = rndrFreeType

newRenderer :: V2 CInt -> IO Renderer
newRenderer winSize = do
  bsc <- Basic.newBasicShader
  prm <- Prim.newPrimitiveShader
  txt <- Text.newTextShader
  freetype <- initFreeType
  let rndr = Renderer bsc prm txt freetype proj view
  -- * Initialize
  setProjection bsc proj
  setProjection prm proj
  setProjection txt proj
  clearRenderArea rndr
  return rndr
  where
    proj = mkOrtho winSize True
    view = Cam.viewMatFromCam Cam.camForVertFlip

freeRenderer :: Renderer -> IO ()
freeRenderer Renderer{..} =
  doneFreeType rndrFreeType
  -- TODO: Implement others

withProjView :: M44 Float -> M44 Float -> Renderer -> (Renderer -> IO ()) -> IO ()
withProjView proj view rndr renderSome = do
  -- Set Projection
  setProjection (rndrBasicShader rndr) proj
  setProjection (rndrPrimShader rndr) proj
  setProjection (rndrTextShader rndr) proj
  --
  renderSome $ rndr {rndrCurrentView = view} -- With viewing matrix
  -- Set projection back
  setProjection (rndrBasicShader rndr) orgProj
  setProjection (rndrPrimShader rndr) orgProj
  setProjection (rndrTextShader rndr) orgProj
  where
    orgProj = rndrBasisProj rndr

clearRenderArea :: Renderer -> IO ()
clearRenderArea rndr =
  Basic.setRenderArea (rndrBasicShader rndr) 0 0 1 1

-- | Discard pixels out of this area (pos and size)
setRenderArea :: Renderer
              -> V2 Int -- ^ Window size
              -> V2 Int -- ^ Position (V2 left top)
              -> V2 Int -- ^ Size
              -> IO ()
setRenderArea rndr winSize pos size =
  Basic.setRenderArea (rndrBasicShader rndr) x0 y0 x1 y1
  where
    V2 winW winH = fromIntegral <$> winSize
    V2 x y = fromIntegral <$> pos
    V2 w h = fromIntegral <$> size
    fx ax = ax / winW -- * 2 - 1
    fy ay = ay / winH -- * 2 - 1
    x0 = fx x           -- left
    y0 = 1 - fy (y + h) -- bottom
    x1 = fx $ x + w     -- right
    y1 = 1 - fy y       -- top

-- | Do action after calling `setRenderArea` and clearRenderArea
withRenderArea :: Renderer -> V2 Int -> V2 Int  -> V2 Int -> IO a -> IO a
withRenderArea rndr winSize pos size action = do
  setRenderArea rndr winSize pos size
  a <- action
  clearRenderArea rndr
  return a

-- | Render Texture with BufferObject of texture coord
-- You can make texture coord buffer object by `newTexCoordVbo`.
renderTextureWithTexCoord :: Renderer -> Texture -> TypedBufferObject TagVec2 -> RContext -> IO ()
renderTextureWithTexCoord Renderer{..} tex texCoordVbo rctx = do
  Basic.setTexCoordVbo rndrBasicShader texCoordVbo
  setTexture rndrBasicShader $ texObject tex
  let model = Render.mkModelMatForNormalized rctx
      modelView = rndrCurrentView !*! model
  Render.setModelView rndrBasicShader modelView
  Render.renderTextureShader rndrBasicShader

-- | Render Texture
-- If call with texture coord, GL.BufferObject will be generated and deleted after rendering.
-- For fast rendering, use renderTextureWithTexCoord.
renderTexture :: Renderer -> Texture -> Maybe (Point V2 Int, V2 Int) -> RContext -> IO ()
renderTexture rndr tex Nothing rctx =
  renderTextureWithTexCoord rndr tex buf rctx
  where
    buf = shdrTexCoordVbo $ rndrBasicShader rndr
renderTexture rndr tex (Just (texCoord, texSize)) rctx = do
  buf <- Texture.newTexCoordVbo tex texCoord texSize -- TODO: Modify original vbo by modeling matrix
  renderTextureWithTexCoord rndr tex buf rctx
  GL.deleteObjectName $ unTBO buf

-- | Render CharTexture list.
renderText :: Foldable t => Renderer -> V2 Int -> t CharTexture -> IO ()
renderText Renderer{..} =
  Render.renderTextTexture rndrTextShader rndrCurrentView

genTextImage :: Foldable t => Renderer -> t CharTexture -> IO Texture
genTextImage Renderer{..} =
  Render.genTextImage_ rndrTextShader rndrBasisProj
