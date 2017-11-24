{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}

module Protonic.TTFHelper
( sizeText
, renderBlended
, GlyphMetrics (..)
, rawGlyphMetrics
, fontFromBytes
) where

import           Control.Exception.Safe  as E
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Text               (Text)
import           Data.Text.Foreign       (lengthWord16, unsafeCopyToPtr)
import           Data.Word               (Word16, Word8)
import           Data.Char               (ord)
import           Foreign.C.Types         (CInt (..), CUShort)
import           Foreign.Marshal.Alloc   (alloca, allocaBytes)
import           Foreign.Marshal.Utils   (with)
import           Foreign.Ptr             (Ptr, castPtr, plusPtr)
import           Foreign.Storable
import           Linear.V4

import qualified SDL.TTF.FFI             as FFI

import qualified SDL
import           SDL.Raw                 (Color (..), Surface, RWops)
import           SDL.Raw.Filesystem      (rwFromConstMem)

#include "ttf_helper.h"

foreign import ccall "TTF_RenderUNICODE_Blended_"
  cRenderUNICODE_Blended :: FFI.TTFFont -> Ptr CUShort -> Ptr Color -> IO (Ptr Surface)

foreign import ccall "TTF_SizeUNICODE"
  cSizeUNICODE :: FFI.TTFFont -> Ptr CUShort -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall "minx_TTF_GlyphMetrics"
  cMinxOfGlyph :: FFI.TTFFont -> CInt -> Ptr GlyphMetrics -> IO CInt

foreign import ccall "TTF_OpenFontRW_"
  cOpenFontRW :: Ptr RWops -> CInt -> IO FFI.TTFFont

-- Reference - https://github.com/sbidin/sdl2-ttf/blob/master/src/SDL/Font.hs
renderBlended :: MonadIO m => FFI.TTFFont -> V4 Word8 -> Text -> m SDL.Surface
renderBlended font (V4 r g b a) text =
  fmap unmanaged . liftIO . withText text $ \ptr ->
    with (Color r g b a) $ \fg ->
      cRenderUNICODE_Blended font (castPtr ptr) fg
  where
    unmanaged p = SDL.Surface p Nothing

withText :: Text -> (Ptr Word16 -> IO a) -> IO a
withText text act =
  allocaBytes len $ \ptr -> do
    unsafeCopyToPtr text ptr
    pokeByteOff ptr (len - 2) (0 :: CUShort)
    act ptr
  where
    len = 2 * (lengthWord16 text + 1)

sizeText :: MonadIO m => FFI.TTFFont -> Text -> m (Int, Int)
sizeText font text =
  liftIO . withText text $ \ptr ->
    alloca $ \w ->
      alloca $ \h ->
        cSizeUNICODE font (castPtr ptr) w h >>= \case
          0 -> do
            w' <- fromIntegral <$> peek w
            h' <- fromIntegral <$> peek h
            return (w', h')
          _ -> E.throwIO $ userError "sizeText (sizeUNICODE)"

-- TTF_GlyphMetrics
-- http://sdl2referencejp.osdn.jp/TTF_GlyphMetrics.html
-- int TTF_GlyphMetrics(TTF_Font *font, Uint16 ch, int *minx, int *maxx, int *miny, int *maxy, int *advance)

data GlyphMetrics = GlyphMetrics
  { minx :: Int
  , maxx :: Int
  , miny :: Int
  , maxy :: Int
  , advance :: Int
  } deriving Show

instance Storable GlyphMetrics where
  sizeOf _ = #{size CGlyphMetrics}

  alignment _ = #{alignment CGlyphMetrics}

  peek ptr =
    GlyphMetrics <$> (toInt <$> #{peek CGlyphMetrics, minx} ptr)
                 <*> (toInt <$> #{peek CGlyphMetrics, maxx} ptr)
                 <*> (toInt <$> #{peek CGlyphMetrics, miny} ptr)
                 <*> (toInt <$> #{peek CGlyphMetrics, maxy} ptr)
                 <*> (toInt <$> #{peek CGlyphMetrics, advance} ptr)
    where
      toInt :: CInt -> Int
      toInt = fromIntegral

  poke ptr (GlyphMetrics minx maxx miny maxy advance) = do
    #{poke CGlyphMetrics, minx} ptr $ conv minx
    #{poke CGlyphMetrics, maxx} ptr $ conv maxx
    #{poke CGlyphMetrics, miny} ptr $ conv miny
    #{poke CGlyphMetrics, maxy} ptr $ conv maxy
    #{poke CGlyphMetrics, advance} ptr $ conv advance
    where
      conv :: Int -> CInt
      conv = fromIntegral

rawGlyphMetrics :: FFI.TTFFont -> Char -> IO GlyphMetrics
rawGlyphMetrics font c =
  with gm0 $ \p -> do
    _result <- cMinxOfGlyph font ch p
    peek p
  where
    gm0 = GlyphMetrics 0 0 0 0 0
    ch = fromIntegral $ ord c

fontFromBytes :: Ptr Word8 -> Int -> Int -> IO FFI.TTFFont
fontFromBytes ptr len fontSize = do
  rw <- rwFromConstMem (castPtr ptr) (fromIntegral len)
  cOpenFontRW rw $ fromIntegral fontSize
