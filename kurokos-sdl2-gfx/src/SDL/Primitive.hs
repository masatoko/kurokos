{-# LANGUAGE OverloadedStrings #-}
module SDL.Primitive where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Int               (Int16)
import           Data.Word              (Word8)
import           Foreign.C.Types        (CInt)

import           SDL.Internal.Exception (throwIfNeg_)
import           SDL.Internal.Types     (Renderer (..))
import           SDL.Vect               (V2 (..), V4 (..))

import qualified SDL.Raw.Primitive

type Width = Word8
type Pos = V2 CInt
type Color = V4 Word8
type Radius = CInt

cint :: CInt -> Int16
cint = fromIntegral

thickLine :: MonadIO m => Renderer -> Pos -> Pos -> Width -> Color -> m ()
thickLine (Renderer p) (V2 x y) (V2 u v) w (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.thickLineRGBA" "thickLineRGBA" $ liftIO $
    SDL.Raw.Primitive.thickLineRGBA
      p (cint x) (cint y) (cint u) (cint v) w r g b a

roundRectangle :: MonadIO m => Renderer -> Pos -> Pos -> Radius -> Color -> m ()
roundRectangle (Renderer p) (V2 x y) (V2 u v) rad (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.roundedRectangleRGBA" "roundedRectangleRGBA" $ liftIO $
    SDL.Raw.Primitive.roundedRectangleRGBA
      p (cint x) (cint y) (cint u) (cint v) (cint rad) r g b a

fillRectangle :: MonadIO m => Renderer -> Pos -> Pos -> Color -> m ()
fillRectangle (Renderer p) (V2 x y) (V2 u v) (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.boxRGBA" "boxRGBA" $ liftIO $
    SDL.Raw.Primitive.boxRGBA
      p (cint x) (cint y) (cint u) (cint v) r g b a

fillRoundRectangle :: MonadIO m => Renderer -> Pos -> Pos -> Radius -> Color -> m ()
fillRoundRectangle (Renderer p) (V2 x y) (V2 u v) rad (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.roundedBoxRGBA" "roundedBoxRGBA" $ liftIO $
    SDL.Raw.Primitive.roundedBoxRGBA
      p (cint x) (cint y) (cint u) (cint v) (cint rad) r g b a

fillCircle :: MonadIO m => Renderer -> Pos -> Radius -> Color -> m ()
fillCircle (Renderer p) (V2 x y) rad (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.filledCircleRGBA" "filledCircleRGBA" $ liftIO $
    SDL.Raw.Primitive.filledCircleRGBA
      p (cint x) (cint y) (cint rad) r g b a

smoothCircle :: MonadIO m => Renderer -> Pos -> Radius -> Color -> m ()
smoothCircle (Renderer p) (V2 x y) rad (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.aacircleRGBA" "aacircleRGBA" $ liftIO $
    SDL.Raw.Primitive.aacircleRGBA
      p (cint x) (cint y) (cint rad) r g b a
