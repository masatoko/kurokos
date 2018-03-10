{-# LANGUAGE TemplateHaskell #-}
module Kurokos.Graphics.Types where

import           Control.Lens
import           Data.Word                 (Word8)
import           Linear

import qualified Graphics.Rendering.OpenGL as GL

data TagBool = TagBool deriving Show
data TagInt = TagInt deriving Show
data TagFloat = TagFloat deriving Show
data TagVec2 = TagVec2 deriving Show
data TagVec3 = TagVec3 deriving Show
data TagVec4 = TagVec4 deriving Show
data TagMat2 = TagMat2 deriving Show
data TagMat3 = TagMat3 deriving Show
data TagMat4 = TagMat4 deriving Show
data TagAryMat4 = TagAryMat4 deriving Show
data TagSampler2D = TagSampler2D GL.TextureUnit deriving Show
data TagSampler2DShadow = TagSampler2DShadow deriving Show
data TagSamplerCube = TagSamplerCube GL.TextureUnit deriving Show

data AttribVar tag = AttribVar tag GL.AttribLocation

data UniformVar tag = UniformVar tag GL.UniformLocation

newtype TypedBufferObject tag = TBO { unTBO :: GL.BufferObject }

type Vec2 = V2 Float

-- | Rendering context
data RContext = RContext
  { rctxCoord     :: V2 Int
  -- ^ Left bottom coord
  , rctxSize      :: V2 Int
  -- ^ Size
  , rctxRot       :: Maybe Float
  -- ^ Rotation angle [rad]
  , rctxRotCenter :: Maybe (V2 Float)
  -- ^ Rotation center coord
  }

-- Texture

data Texture = Texture
  { texObject :: GL.TextureObject
  , texWidth  :: Int
  , texHeight :: Int
  }

-- Font

type Color = V4 Word8

type FontSize = Int

type TextTexture = [CharTexture]

data CharTexture = CharTexture
  { ctTexture   :: Texture
  , _ctColor    :: V4 Float
  , _ctFontSize :: FontSize
  , _ctLeft     :: Int
  , _ctTop      :: Int
  , _ctAdvanceX :: Float -- ^ horiAdvance [FT_Glyph_Metrics](https://hackage.haskell.org/package/freetype2-0.1.2/docs/Graphics-Rendering-FreeType-Internal-GlyphMetrics.html#t:FT_Glyph_Metrics)
  , _ctAdvanceY :: Float -- ^ vertAdvance
  }

makeLenses ''CharTexture

--

data ProjectionType
  = Ortho
  | Frustum Float Float -- Near Far
  deriving (Eq, Show)
