module Kurokos.Graphics.Font where

import           Control.Monad                                       (foldM,
                                                                      foldM_,
                                                                      forM,
                                                                      unless)
import           Data.ByteString.Internal                            (ByteString (..))
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

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types                                     (CChar (..))
import qualified Graphics.Rendering.FreeType.Internal                as FT
import qualified Graphics.Rendering.FreeType.Internal.Bitmap         as FT
import qualified Graphics.Rendering.FreeType.Internal.BitmapSize     as FTS
import qualified Graphics.Rendering.FreeType.Internal.Face           as FT
import qualified Graphics.Rendering.FreeType.Internal.FaceType       as FT
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot      as FT
import qualified Graphics.Rendering.FreeType.Internal.Library        as FT
import qualified Graphics.Rendering.FreeType.Internal.PrimitiveTypes as FT

-- Reffered this article [http://zyghost.com/articles/Haskell-font-rendering-with-freetype2-and-opengl.html].
-- Original code is [https://github.com/schell/editor/blob/glyph-rendering/src/Graphics/Text/Font.hs].
-- Thanks to schell.

loadCharacter :: FilePath -> Char -> Int -> IO GL.TextureObject
loadCharacter path char px = do
    -- FreeType (http://freetype.org/freetype2/docs/tutorial/step1.html)
    ft <- freeType

    -- Get the Ubuntu Mono fontface.
    ff <- fontFace ft path
    runFreeType $ FT.ft_Set_Pixel_Sizes ff (fromIntegral px) 0

    -- Get the unicode char index.
    chNdx <- FT.ft_Get_Char_Index ff $ fromIntegral $ fromEnum char

    -- Load the glyph into freetype memory.
    runFreeType $ FT.ft_Load_Glyph ff chNdx FT.ft_LOAD_DEFAULT

    -- Get the GlyphSlot.
    slot <- peek $ FT.glyph ff

    -- Number of glyphs
    n <- peek $ FT.num_glyphs ff
    putStrLn $ "glyphs:" ++ show n

    fmt <- peek $ FT.format slot
    putStrLn $ "glyph format:" ++ glyphFormatString fmt

    -- This is [] for Ubuntu Mono, but I'm guessing for bitmap
    -- fonts this would be populated with the different font
    -- sizes.
    putStr "Sizes:"
    numSizes <- peek $ FT.num_fixed_sizes ff
    sizesPtr <- peek $ FT.available_sizes ff
    sizes <- forM [0 .. numSizes-1] $ \i ->
        peek $ sizesPtr `plusPtr` fromIntegral i :: IO FTS.FT_Bitmap_Size
    print sizes

    l <- peek $ FT.bitmap_left slot
    t <- peek $ FT.bitmap_top slot
    putStrLn $ concat [ "left:"
                      , show l
                      , "\ntop:"
                      , show t
                      ]

    runFreeType $ FT.ft_Render_Glyph slot FT.ft_RENDER_MODE_NORMAL

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

    putStrLn $ "padding by " ++ show p

    -- Get the raw bitmap data.
    bmpData <- peekArray (w*h) $ FT.buffer bmp

    let data' = addPadding p w 0 bmpData
        -- data'' = concat $ map toRGBA data'
    data'' <- makeRGBABytes (V3 255 0 255) data'

    -- Set the texture params on our bound texture.
    GL.texture GL.Texture2D $= GL.Enabled

    -- Generate an opengl texture.
    tex <- newBoundTexUnit 0
    printError
    --
    putStrLn "Buffering glyph bitmap into texture."
    let (PS fptr off len) = data''
    let pokeColor ptr _ = do
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

    printError

    putStrLn "Texture loaded."
    GL.textureFilter   GL.Texture2D   $= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)

    return tex

newBoundTexUnit :: Int -> IO GL.TextureObject
newBoundTexUnit u = do
  [tex] <- GL.genObjectNames 1
  GL.texture GL.Texture2D $= GL.Enabled
  GL.activeTexture $= GL.TextureUnit (fromIntegral u)
  GL.textureBinding GL.Texture2D $= Just tex
  return tex

addPadding :: Int -> Int -> a -> [a] -> [a]
addPadding _ _ _ [] = []
addPadding amt w val xs = a ++ b ++ c
    where a = take w xs
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
    | fmt == FT.ft_GLYPH_FORMAT_OUTLINE = "ft_GLYPH_FORMAT_OUTLINE"
    | fmt == FT.ft_GLYPH_FORMAT_PLOTTER = "ft_GLYPH_FORMAT_PLOTTER"
    | fmt == FT.ft_GLYPH_FORMAT_BITMAP = "ft_GLYPH_FORMAT_BITMAP"
    | otherwise = "ft_GLYPH_FORMAT_NONE"


runFreeType :: IO FT.FT_Error -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error:" ++ show r


freeType :: IO FT.FT_Library
freeType = alloca $ \p -> do
    runFreeType $ FT.ft_Init_FreeType p
    peek p


fontFace :: FT.FT_Library -> FilePath -> IO FT.FT_Face
fontFace ft fp = withCString fp $ \str ->
    alloca $ \ptr -> do
        runFreeType $ FT.ft_New_Face ft str 0 ptr
        peek ptr

printError :: IO ()
printError = get GL.errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)
