-- | Font utility for freetype2
--
-- @
-- import qualified Kurokos.Graphics.Font as Font
-- import qualified Kurokos.Graphics      as G
--
-- ft <- Font.initFreeType
-- face <- Font.newFace ft "_test/mplus-1p-medium.ttf"
--
-- text1 <- E.bracket (G.createTextTexture face 16 (V3 255 0 0) "Hello, ") G.deleteTextTexture
-- text2 <- E.bracket (G.createTextTexture face 16 (V3 0 0 255) "World!") G.deleteTextTexture
-- let textTexture = text1 ++ text2
--
-- -- G.renderText (V2 x0 y0) TextShader textTexture
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
  -- ** Font Face
  , Font
  , newFace
  , newFaceBS
  , doneFace
  ) where

import qualified Control.Exception                                   as E
import           Control.Monad                                       (foldM_,
                                                                      unless)
import           Control.Monad.IO.Class                              (MonadIO,
                                                                      liftIO)
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

import           Kurokos.Graphics.Util                               (throwIfNot0)

type Font = FT.FT_Face

--
withFreeType :: MonadIO m => (FT.FT_Library -> IO a) -> m a
withFreeType = liftIO . E.bracket initFreeType doneFreeType

initFreeType :: MonadIO m => m FT.FT_Library
initFreeType = liftIO $ alloca $ \p -> do
  throwIfNot0 "ft_Init_FreeType" $ FT.ft_Init_FreeType p
  peek p

doneFreeType :: MonadIO m => FT.FT_Library -> m ()
doneFreeType ft = liftIO $
  throwIfNot0 "ft_Done_FreeType" $ FT.ft_Done_FreeType ft
--

--
newFace :: MonadIO m => FT.FT_Library -> FilePath -> m FT.FT_Face
newFace ft fp = liftIO $ withCString fp $ \str ->
  alloca $ \ptr -> do
    throwIfNot0 "ft_New_Face" $ FT.ft_New_Face ft str 0 ptr
    peek ptr

newFaceBS :: MonadIO m => FT.FT_Library -> BS.ByteString -> m FT.FT_Face
newFaceBS ft (PS fptr _off len) = liftIO $
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

doneFace :: MonadIO m => FT.FT_Face -> m ()
doneFace face = liftIO $
  throwIfNot0 "ft_Done_Face" $ FT.ft_Done_Face face
