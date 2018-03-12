module Kurokos.Graphics.Util where

import qualified Control.Exception         as E
import           Control.Monad             (unless)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Foreign.C.Types           (CInt)
import           Foreign.Ptr               (nullPtr)
import           Linear.V2

import qualified Graphics.GLUtil           as GLU
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

makeVAO :: IO a -> IO (a, GL.VertexArrayObject)
makeVAO setup = do
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  a <- setup
  GL.bindVertexArrayObject $= Nothing
  return (a, vao)

throwIfNot0 :: String -> IO CInt -> IO ()
throwIfNot0 tag m = do
  r <- m
  unless (r == 0) $
    E.throwIO $ userError $ "throwIfNot0 @" ++ tag ++ ": " ++ show r

makeRenderFBO :: V2 GL.GLsizei -> IO (GL.FramebufferObject, GL.TextureObject)
makeRenderFBO size@(V2 sizeW sizeH) = do
  -- Generate FBO
  fbo <- GL.genObjectName
  withFBO fbo size $ do
    -- Generate Texture
    renderTex <- GL.genObjectName
    GL.textureBinding GL.Texture2D $= Just renderTex
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA' (GL.TextureSize2D sizeW sizeH) 0 (GL.PixelData GL.RGBA GL.UnsignedByte nullPtr)
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
    GLU.texture2DWrap $= (GL.Mirrored, GL.ClampToEdge)
    GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment colorAttachmentId) GL.Texture2D renderTex 0 -- Bind texture to FBO
    -- Generate depth buffer
    depthBuffer <- GL.genObjectName
    GL.bindRenderbuffer GL.Renderbuffer $= depthBuffer
    GL.renderbufferStorage GL.Renderbuffer GL.DepthComponent' (GL.RenderbufferSize sizeW sizeH)
    GL.framebufferRenderbuffer GL.Framebuffer GL.DepthAttachment GL.Renderbuffer depthBuffer
    --
    GL.drawBuffers $= [GL.FBOColorAttachment fboColorAttachmentId]
    return (fbo, renderTex)
  where
    colorAttachmentId = 0
    fboColorAttachmentId = 0

withFBO :: MonadIO m => GL.FramebufferObject -> V2 GL.GLsizei -> m a -> m a
withFBO fbo (V2 w h) action =
  GLU.withViewport (GL.Position 0 0) (GL.Size w h) $ do
    originalFBO <- liftIO $ GL.get $ GL.bindFramebuffer GL.Framebuffer
    liftIO $ GL.bindFramebuffer GL.Framebuffer $= fbo
    ret <- action
    liftIO $ GL.bindFramebuffer GL.Framebuffer $= originalFBO
    return ret

withBlend :: MonadIO m => m a -> m a
withBlend action = do
  org <- liftIO $ GL.get GL.blendFuncSeparate
  liftIO $ GL.blendFuncSeparate $= ((GL.SrcAlpha, GL.OneMinusSrcAlpha), (GL.One, GL.One))
  ret <- action
  liftIO $ GL.blendFuncSeparate $= org
  return ret
