module Kurokos.Graphics.Types where

import           Data.Word                 (Word8)
import           Linear

import qualified Graphics.Rendering.OpenGL as GL

type ActiveTextureIndex = GL.GLuint

data TagVec2 = TagVec2 deriving Show
data TagVec3 = TagVec3 deriving Show
data TagVec4 = TagVec4 deriving Show
data TagMat2 = TagMat2 deriving Show
data TagMat3 = TagMat3 deriving Show
data TagMat4 = TagMat4 deriving Show
data TagSampler2D = TagSampler2D ActiveTextureIndex deriving Show

data AttribVar tag = AttribVar tag GL.AttribLocation

data UniformVar tag = UniformVar tag GL.UniformLocation

type Vec2 = V2 Float

-- Texture

data Texture = Texture
  { texObject :: GL.TextureObject
  , texWidth  :: Int
  , texHeight :: Int
  }

-- Font

type Color3 = V3 Word8

type TextTexture = [CharTexture]

data CharTexture = CharTexture
  { ctTexture  :: Texture
  , ctColor    :: Color3
  , ctLeft     :: Int
  , ctTop      :: Int
  , ctAdvanceX :: Float -- ^ horiAdvance [FT_Glyph_Metrics](https://hackage.haskell.org/package/freetype2-0.1.2/docs/Graphics-Rendering-FreeType-Internal-GlyphMetrics.html#t:FT_Glyph_Metrics)
  , ctAdvanceY :: Float -- ^ vertAdvance
  }

--

data ProjectionType
  = Ortho
  | Frustum Float Float -- Near Far
  deriving (Eq, Show)
