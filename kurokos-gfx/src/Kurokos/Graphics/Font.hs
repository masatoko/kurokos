-- | Font utility for freetype2
--
-- @
-- import qualified Kurokos.Graphics.Font as Font
-- import qualified Kurokos.Graphics      as G
--
-- ft <- Font.initFreeType
-- face <- Font.newFace ft "_test/mplus-1p-medium.ttf"
-- Font.setPixelSize face 32
--
-- text1 <- E.bracket (G.createTextTexture face (V3 255 0 0) "Hello, ") G.deleteTextTexture
-- text2 <- E.bracket (G.createTextTexture face (V3 0 0 255) "World!") G.deleteTextTexture
-- let texttex = text1 ++ text2
--
-- -- G.renderText (V2 x0 y0) TextChader texttex
--
-- G.deleteTexture text1
-- G.deleteTexture text2
-- Font.doneFace face
-- Font.doneFreeType ft
-- @
--

module Kurokos.Graphics.Font
  (
  -- ** Initialize FreeType
    withFreeType
  , initFreeType
  , doneFreeType
  -- ** Face
  , newFace
  , newFaceBS
  , doneFace
  , setPixelSize
  ) where

import qualified Control.Exception                                   as E
import           Control.Monad                                       (foldM_,
                                                                      unless)
import qualified Data.ByteString                                     as BS
import           Data.ByteString.Internal                            (ByteString (..))
import           Foreign                                             (Ptr,
                                                                      alloca,
                                                                      allocaBytes)
import           Foreign.C.String                                    (withCString)
import           Foreign.C.Types                                     (CUChar (..))
import           Foreign.ForeignPtr                                  (withForeignPtr)
import           Foreign.Ptr                                         (plusPtr)
import           Foreign.Storable                                    (peek,
                                                                      poke)
import qualified Graphics.Rendering.FreeType.Internal                as FT
import qualified Graphics.Rendering.FreeType.Internal.Face           as FT
import qualified Graphics.Rendering.FreeType.Internal.Library        as FT
import qualified Graphics.Rendering.FreeType.Internal.PrimitiveTypes as FT

--
withFreeType :: (FT.FT_Library -> IO a) -> IO a
withFreeType = E.bracket initFreeType doneFreeType

initFreeType :: IO FT.FT_Library
initFreeType = alloca $ \p -> do
  throwIfNot0 $ FT.ft_Init_FreeType p
  peek p

doneFreeType :: FT.FT_Library -> IO ()
doneFreeType ft = throwIfNot0 $ FT.ft_Done_FreeType ft
--

--
newFace :: FT.FT_Library -> FilePath -> IO FT.FT_Face
newFace ft fp = withCString fp $ \str ->
  alloca $ \ptr -> do
    throwIfNot0 $ FT.ft_New_Face ft str 0 ptr
    peek ptr

newFaceBS :: FT.FT_Library -> BS.ByteString -> IO FT.FT_Face
newFaceBS ft (PS fptr _off len) =
  -- Is there any smart way?
  allocaBytes len $ \dst0 -> do -- Destination (Ptr CUChar8)
    withForeignPtr fptr $ \org0 -> -- Origin (Ptr Word8)
      foldM_ work (org0, dst0 :: Ptr CUChar) $ take len [(0::Int)..]
    alloca $ \ptr -> do
      FT.ft_New_Memory_Face ft dst0 (fromIntegral len) 0 ptr
      peek ptr
  where
    work (from, to) _ = do
      poke to . fromIntegral =<< peek from
      return (from `plusPtr` 1, to `plusPtr` 1)

doneFace :: FT.FT_Face -> IO ()
doneFace face = throwIfNot0 $ FT.ft_Done_Face face
--

setPixelSize :: FT.FT_Face -> Int -> IO ()
setPixelSize face size =
  throwIfNot0 $ FT.ft_Set_Pixel_Sizes face (fromIntegral size) 0

throwIfNot0 :: IO FT.FT_Error -> IO ()
throwIfNot0 m = do
  r <- m
  unless (r == 0) $
    E.throwIO $ userError $ "FreeType Error:" ++ show r
