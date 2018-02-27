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
  -- , setTextColor
  -- ** Types
  , Color
  , FontSize
  , ProjectionType (..)
  , Texture (..)
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
import           Kurokos.Renderer
