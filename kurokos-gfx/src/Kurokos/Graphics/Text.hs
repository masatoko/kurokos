module Kurokos.Graphics.Text
  (
  -- ** CharTexture
    createCharTexture
  , deleteCharTexture
  , createTextTexture
  , deleteTextTexture
  ) where

import           Control.Monad                                       (foldM,
                                                                      foldM_)
import           Data.ByteString.Internal                            (ByteString (..))
import qualified Data.Text                                           as T
import           Foreign.ForeignPtr                                  (withForeignPtr)
import           Foreign.Ptr                                         (plusPtr)
import           Foreign.Storable                                    (peek,
                                                                      poke)
import           GHC.ForeignPtr                                      (mallocPlainForeignPtrBytes)

import qualified Data.ByteString                                     as BS
import           Graphics.Rendering.OpenGL                           (($=))
import qualified Graphics.Rendering.OpenGL                           as GL

import           Foreign                                             (Ptr,
                                                                      Word8,
                                                                      peekArray)
import           Foreign.C.Types                                     (CChar (..))
import qualified Graphics.GLUtil                                     as GLU
import qualified Graphics.Rendering.FreeType.Internal                as FT
import qualified Graphics.Rendering.FreeType.Internal.Bitmap         as FT
import qualified Graphics.Rendering.FreeType.Internal.Face           as FT
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot      as FT
import qualified Graphics.Rendering.FreeType.Internal.PrimitiveTypes as FT
import qualified Graphics.Rendering.FreeType.Internal.Vector         as FT

import           Kurokos.Graphics.Font                               (Font)
import           Kurokos.Graphics.Texture                            (deleteTexture)
import           Kurokos.Graphics.Types                              (CharTexture (..),
                                                                      Color,
                                                                      FontSize,
                                                                      TextTexture,
                                                                      Texture (..))
import           Kurokos.Graphics.Util                               (throwIfNot0)

-- Reffered this article [http://zyghost.com/articles/Haskell-font-rendering-with-freetype2-and-opengl.html].
-- Original code is [https://github.com/schell/editor/blob/glyph-rendering/src/Graphics/Text/Font.hs].
-- Thanks to schell.

deleteCharTexture :: CharTexture -> IO ()
deleteCharTexture = deleteTexture . ctTexture

deleteTextTexture :: TextTexture -> IO ()
deleteTextTexture = mapM_ deleteCharTexture

createTextTexture :: Font -> FontSize -> Color -> T.Text -> IO TextTexture
createTextTexture font size color =
  mapM work . T.unpack
  where
    work = createCharTexture font size color

createCharTexture :: Font -> FontSize -> Color -> Char -> IO CharTexture
createCharTexture (_, face) size color char = do
  throwIfNot0 "ft_Set_Pixel_Sizes" $ FT.ft_Set_Pixel_Sizes face (fromIntegral size) 0
  --
  charInd <- FT.ft_Get_Char_Index face $ fromIntegral $ fromEnum char -- Get the unicode char index.
  throwIfNot0 "ft_Load_Glyph" $ FT.ft_Load_Glyph face charInd FT.ft_LOAD_DEFAULT -- Load the glyph into freetype memory.
  slot <- peek $ FT.glyph face -- GlyphSlot

  throwIfNot0 "ft_Render_Glyph" $ FT.ft_Render_Glyph slot FT.ft_RENDER_MODE_NORMAL

  -- Get the char bitmap.
  bmp <- peek $ FT.bitmap slot
  -- putStrLn $ concat ["width:"
  --                   , show $ FT.width bmp
  --                   , " rows:"
  --                   , show $ FT.rows bmp
  --                   , " pitch:"
  --                   , show $ FT.pitch bmp
  --                   , " num_grays:"
  --                   , show $ FT.num_grays bmp
  --                   , " pixel_mode:"
  --                   , show $ FT.pixel_mode bmp
  --                   , " palette_mode:"
  --                   , show $ FT.palette_mode bmp
  --                   ]

  let w  = fromIntegral $ FT.width bmp
      h  = fromIntegral $ FT.rows bmp

  bmpData <- peekArray (w * h) $ FT.buffer bmp
  bytes <- convToByteString bmpData

  -- Generate an opengl texture.
  tex <- newBoundTexUnit 0
  GLU.printError
  --
  -- Buffering glyph bitmap into texture.
  let (PS fptr off len) = bytes
      pokeColor ptr _ = do
        GL.texImage2D
          GL.Texture2D
          GL.NoProxy
          0
          GL.RGBA8 -- PixelInternalFormat
          (GL.TextureSize2D (fromIntegral w) (fromIntegral h))
          0
          (GL.PixelData GL.RGBA GL.UnsignedByte ptr) -- PixelFormat
        return $ ptr `plusPtr` off
  withForeignPtr fptr $ \ptr0 ->
    foldM_ pokeColor ptr0 $ take len [(0::Int)..]
  GLU.printError

  GL.textureFilter   GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)
  --
  left <- fromIntegral <$> peek (FT.bitmap_left slot)
  top  <- fromIntegral <$> peek (FT.bitmap_top slot)
  FT.FT_Vector advanceX advanceY <- peek $ FT.advance slot
  return $ CharTexture
    (Texture tex w h) color' size left top
    (fromIntegral advanceX / 64)
    (fromIntegral advanceY / 64)
  where
    color' = (/ 255) . fromIntegral <$> color

newBoundTexUnit :: Int -> IO GL.TextureObject
newBoundTexUnit u = do
  tex <- GL.genObjectName
  -- GL.texture GL.Texture2D $= GL.Enabled
  GL.activeTexture $= GL.TextureUnit (fromIntegral u)
  GL.textureBinding GL.Texture2D $= Just tex
  return tex

convToByteString :: [CChar] -> IO BS.ByteString
convToByteString cs =
  create (length cs * 4) $ \ptr0 ->
    foldM_ work ptr0 cs
  where
    work p0 (CChar a) =
      foldM pokeIncr p0 [255, 255, 255, fromIntegral a]
      where
        pokeIncr p depth = do
          poke p depth
          return $ p `plusPtr` 1

    create :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString
    create l f = do
      fp <- mallocPlainForeignPtrBytes l
      withForeignPtr fp $ \p -> f p
      return $! PS fp 0 l
