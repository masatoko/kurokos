module Kurokos.Graphics.Font where

import qualified Control.Exception                                   as E
import           Control.Monad                                       (foldM,
                                                                      foldM_,
                                                                      forM,
                                                                      unless)
import           Data.ByteString.Internal                            (ByteString (..))
import qualified Data.Text                                           as T
import qualified Data.Vector                                         as V
import           Foreign.ForeignPtr                                  (withForeignPtr)
import           Foreign.Ptr                                         (plusPtr)
import           Foreign.Storable                                    (peek,
                                                                      poke)
import           GHC.ForeignPtr                                      (mallocPlainForeignPtrBytes)
import           Linear.V3                                           (V3 (..))
import           System.IO                                           (hPutStrLn,
                                                                      stderr)

import qualified Data.ByteString                                     as BS
import           Graphics.Rendering.OpenGL                           (get, ($=))
import qualified Graphics.Rendering.OpenGL                           as GL

import           Foreign                                             (Ptr,
                                                                      Word8,
                                                                      alloca,
                                                                      allocaBytes,
                                                                      peekArray,
                                                                      realloc,
                                                                      reallocBytes)
import           Foreign.C.String                                    (withCString)
import           Foreign.C.Types                                     (CChar (..),
                                                                      CUChar (..))
import qualified Graphics.GLUtil                                     as GLU
import qualified Graphics.Rendering.FreeType.Internal                as FT
import qualified Graphics.Rendering.FreeType.Internal.Bitmap         as FT
import qualified Graphics.Rendering.FreeType.Internal.BitmapSize     as FTS
import qualified Graphics.Rendering.FreeType.Internal.Face           as FT
import qualified Graphics.Rendering.FreeType.Internal.FaceType       as FT
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot      as FT
import qualified Graphics.Rendering.FreeType.Internal.Library        as FT
import qualified Graphics.Rendering.FreeType.Internal.PrimitiveTypes as FT

import           Kurokos.Graphics.Types                              (CharTexture (..),
                                                                      TextTexture,
                                                                      Texture (..))

-- Reffered this article [http://zyghost.com/articles/Haskell-font-rendering-with-freetype2-and-opengl.html].
-- Original code is [https://github.com/schell/editor/blob/glyph-rendering/src/Graphics/Text/Font.hs].
-- Thanks to schell.

createTextTexture :: FT.FT_Face -> V3 Word8 -> T.Text -> IO TextTexture
createTextTexture face color =
  fmap V.fromList . mapM work . T.unpack
  where
    work = createCharTexture face color

createCharTexture :: FT.FT_Face -> V3 Word8 -> Char -> IO CharTexture
createCharTexture face color char = do
  charInd <- FT.ft_Get_Char_Index face $ fromIntegral $ fromEnum char -- Get the unicode char index.
  throwIfNot0 $ FT.ft_Load_Glyph face charInd FT.ft_LOAD_DEFAULT -- Load the glyph into freetype memory.
  slot <- peek $ FT.glyph face -- GlyphSlot

  throwIfNot0 $ FT.ft_Render_Glyph slot FT.ft_RENDER_MODE_NORMAL

  -- Get the char bitmap.
  bmp <- peek $ FT.bitmap slot
  putStrLn $ concat ["width:"
                    , show $ FT.width bmp
                    , " rows:"
                    , show $ FT.rows bmp
                    , " pitch:"
                    , show $ FT.pitch bmp
                    , " num_grays:"
                    , show $ FT.num_grays bmp
                    , " pixel_mode:"
                    , show $ FT.pixel_mode bmp
                    , " palette_mode:"
                    , show $ FT.palette_mode bmp
                    ]

  let w  = fromIntegral $ FT.width bmp
      h  = fromIntegral $ FT.rows bmp
      w' = fromIntegral w :: Integer
      h' = fromIntegral h
      p  = 4 - w `mod` 4
      nw = p + fromIntegral w'

  bmpData <- peekArray (w*h) $ FT.buffer bmp
  bytes <- makeRGBABytes color $ addPadding p w 0 bmpData

  -- Set the texture params on our bound texture.
  GL.texture GL.Texture2D $= GL.Enabled

  -- Generate an opengl texture.
  tex <- newBoundTexUnit 0
  GLU.printError
  --
  putStrLn "Buffering glyph bitmap into texture."
  let (PS fptr off len) = bytes
      pokeColor ptr _ = do
        GL.texImage2D
          GL.Texture2D
          GL.NoProxy
          0
          GL.RGBA8 -- PixelInternalFormat
          (GL.TextureSize2D (fromIntegral nw) h')
          0
          (GL.PixelData GL.RGBA GL.UnsignedByte ptr) -- PixelFormat
        return $ ptr `plusPtr` off
  withForeignPtr fptr $ \ptr0 ->
    foldM_ pokeColor ptr0 $ take len [0..]
  GLU.printError

  GL.textureFilter   GL.Texture2D   $= ((GL.Linear', Nothing), GL.Linear')
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)

  CharTexture (Texture tex w h)
    <$> (fromIntegral <$> peek (FT.bitmap_left slot))
    <*> (fromIntegral <$> peek (FT.bitmap_top slot))

newBoundTexUnit :: Int -> IO GL.TextureObject
newBoundTexUnit u = do
  [tex] <- GL.genObjectNames 1
  GL.texture GL.Texture2D $= GL.Enabled
  GL.activeTexture $= GL.TextureUnit (fromIntegral u)
  GL.textureBinding GL.Texture2D $= Just tex
  return tex

addPadding :: Int -> Int -> a -> [a] -> [a]
addPadding _   _ _   [] = []
addPadding amt w val xs = a ++ b ++ c
  where
    a = take w xs
    b = replicate amt val
    c = addPadding amt w val (drop w xs)

makeRGBABytes :: V3 Word8 -> [CChar] -> IO BS.ByteString
makeRGBABytes (V3 r g b) cs =
  create (length cs * 4) $ \ptr0 ->
    foldM_ work ptr0 cs
  where
    work p0 (CChar a) =
      foldM poke' p0 [r,g,b, fromIntegral a]
      where
        poke' p depth = do
          poke p depth
          return $ p `plusPtr` 1

    create :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString
    create l f = do
      fp <- mallocPlainForeignPtrBytes l
      withForeignPtr fp $ \p -> f p
      return $! PS fp 0 l

glyphFormatString :: FT.FT_Glyph_Format -> String
glyphFormatString fmt
  | fmt == FT.ft_GLYPH_FORMAT_COMPOSITE = "ft_GLYPH_FORMAT_COMPOSITE"
  | fmt == FT.ft_GLYPH_FORMAT_OUTLINE   = "ft_GLYPH_FORMAT_OUTLINE"
  | fmt == FT.ft_GLYPH_FORMAT_PLOTTER   = "ft_GLYPH_FORMAT_PLOTTER"
  | fmt == FT.ft_GLYPH_FORMAT_BITMAP    = "ft_GLYPH_FORMAT_BITMAP"
  | otherwise                           = "ft_GLYPH_FORMAT_NONE"

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
newFaceBS ft bytes@(PS fptr off len) =
  -- Is there any smart way?
  allocaBytes len $ \dst0 -> do -- Destination (Ptr CUChar8)
    withForeignPtr fptr $ \org0 -> -- Origin (Ptr Word8)
      foldM_ work (org0, dst0 :: Ptr CUChar) $ take len [0..]
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
