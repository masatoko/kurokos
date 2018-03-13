module Kurokos.Graphics
  (
  -- ** Renderer
    Renderer
  , getFreeType
  , newRenderer
  , freeRenderer
  , withProjView
  , RContext (..)
  , renderTexture
  , renderText
  , genTextImage
  -- , setTextColor
  -- ** Types
  , Color
  , FontSize
  , ProjectionType (..)
  , Texture (..)
  , texSize
  , TextTexture
  , CharTexture
  , ctColor
  , ctFontSize
  , ctLeft
  , ctTop
  , ctAdvanceX
  , ctAdvanceY
  -- ** Texture
  , readTexture
  , decodeTexture
  , decodeTexInfo
  , deleteTexture
  , reloadTextureWith
  -- ** Text
  , createCharTexture
  , deleteCharTexture
  , createTextTexture
  , deleteTextTexture
  -- ** Primitive
  , Prim
  , drawPrim
  , setPrimColor
  , newPrim
  , freePrim
  , newRectangle
  , newFillRectangle
  -- ** Util
  , withBlend
  -- ** Rendering (Temporal)
  , renderTextureShader
  , renderTextTexture
  ) where

import           Kurokos.Graphics.Primitive
import           Kurokos.Graphics.Render
import           Kurokos.Graphics.Text
import           Kurokos.Graphics.Texture
import           Kurokos.Graphics.Texture.Util
import           Kurokos.Graphics.Types
import           Kurokos.Graphics.Util         (withBlend)
import           Kurokos.Renderer
