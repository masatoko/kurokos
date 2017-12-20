module Kurokos.Graphics.Types where

import Linear

import qualified Graphics.Rendering.OpenGL as GL

type ActiveTextureIndex = GL.GLuint

data TagVec2 = TagVec2 deriving Show
data TagVec3 = TagVec3 deriving Show
data TagVec4 = TagVec4 deriving Show
data TagMat2 = TagMat2 deriving Show
data TagMat3 = TagMat3 deriving Show
data TagMat4 = TagMat4 deriving Show
data TagSampler2D = TagSampler2D ActiveTextureIndex deriving Show

data ShaderVar tag loc
  = ShaderVar tag loc

type Vec2 = V2 Float
