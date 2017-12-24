{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Kurokos.Core
  ( KurokosConfig (..)
  , KurokosEnv (..)
  , KurokosState
  , KurokosData
  , KurokosT
  , runKurokos
  , withKurokos
  , Scene (..)
  , runScene
  , SceneState
  , Update
  , Render
  , Transit
  , Transition (..)
  , continue
  , end
  --
  , printDebug
  , getWindowSize
  , getWindow
  , getEvents
  , getFrame
  , getJoysticks
  , showMessageBox
  , getRenderer
  , withRenderer
  ) where

import qualified Control.Concurrent.MVar     as MVar
import qualified Control.Exception           as E
import           Control.Monad               (foldM_, when)
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Managed       (managed, managed_, runManaged)
import           Control.Monad.Reader        (MonadReader, MonadTrans (..),
                                              ReaderT, asks, runReaderT)
import           Control.Monad.State         (MonadState, StateT, evalStateT,
                                              gets, modify, modify')
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Data.Char                   (chr, ord)
import           Data.Maybe                  (mapMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import           Data.Word                   (Word32)
import           Text.Printf                 (printf)

import           Graphics.Rendering.OpenGL   (get, ($=))
import qualified Graphics.Rendering.OpenGL   as GL
import           Linear.V2
import           Linear.V4
import qualified SDL

import qualified Kurokos.Asset               as Asset
import qualified Kurokos.Asset.Raw           as Asset
import           Kurokos.Exception           (KurokosException (..))
import qualified Kurokos.Graphics            as G
import qualified Kurokos.Graphics.Font       as Font
import           Kurokos.Types
import           Kurokos.UI                  (RenderEnv (..))

data KurokosConfig = KurokosConfig
  { confWinTitle         :: Text
  -- ^ Tile on the title bar of the window
  , confDebugPrintFPS    :: Bool
  -- ^ Print FPS for debugging
  , confDebugPrintSystem :: Bool
  -- ^ Print system information for debugging
  , confSystemRawAsset   :: Asset.RawAssetManager
  , confSystemFontId     :: Asset.Ident
  }

type Frame = Word32

data KurokosEnv = KurokosEnv
  { envGraphFps         :: Int
  , envWindow           :: SDL.Window
  , envMRenderer        :: MVar.MVar G.Renderer
  , envGLContext        :: SDL.GLContext
  -- Resource
  , envSystemAssets     :: Asset.AssetManager
  , envSystemFont       :: Font.Font
  , envAsciiTextures    :: V.Vector G.CharTexture
  -- Debug
  , envDebugPrintFps    :: Bool
  , envDebugPrintSystem :: Bool
  }

data KurokosState = KurokosState
  {
    kstMessages   :: [String]
  , kstSdlEvents  :: [SDL.Event]
  , kstSceneState :: SceneState
  , kstJoysticks  :: V.Vector Joystick
  --
  , kstStart      :: !Frame
  , kstCount      :: !Int
  --
  , kstActualFps  :: !Double
  }

newtype KurokosData = KurokosData (KurokosEnv, KurokosState)

newtype KurokosT m a = KurokosT {
    runKT :: ReaderT KurokosEnv (StateT KurokosState m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader KurokosEnv, MonadState KurokosState, MonadBase base)

runKurokos :: Monad m => KurokosData -> KurokosT m a -> m a
runKurokos (KurokosData (conf, stt)) k =
  evalStateT (runReaderT (runKT k) conf) stt

instance MonadTrans KurokosT where
  lift = KurokosT . lift . lift

instance MonadTransControl KurokosT where
  type StT KurokosT a = (a, KurokosState)
  liftWith f = KurokosT $
    liftWith $ \runS ->
      liftWith $ \runR ->
        f $ \ma -> runR $ runS $ runKT ma
  restoreT = KurokosT . restoreT . restoreT

instance MonadBaseControl base m => MonadBaseControl base (KurokosT m) where
  type StM (KurokosT m) a = ComposeSt KurokosT m a
  liftBaseWith            = defaultLiftBaseWith
  restoreM                = defaultRestoreM

-- newtype KurokosEnvT a = KurokosEnvT {
--     runKET :: ReaderT KurokosEnv IO a
--   } deriving (Functor, Applicative, Monad, MonadIO, MonadReader KurokosEnv, MonadThrow, MonadCatch, MonadMask)
--
-- runKurokosEnvT :: KurokosEnv -> KurokosEnvT a -> IO a
-- runKurokosEnvT conf k = runReaderT (runKET k) conf

withKurokos :: KurokosConfig -> SDL.WindowConfig -> (KurokosData -> IO ()) -> IO ()
withKurokos KurokosConfig{..} winConf go =
  runManaged $ do
    managed_ withSDL
    (win, r, glContext) <- managed withWinRenderer
    assetManager <- managed $ E.bracket (Asset.newAssetManager r confSystemRawAsset)
                                        Asset.freeAssetManager
    font <- case Asset.lookupFont confSystemFontId assetManager of
              Nothing   -> liftIO $ E.throwIO $ userError $ "Missing system font: " ++ T.unpack confSystemFontId
              Just font -> return font
    ascii <- managed $
      E.bracket (V.fromList <$> G.createTextTexture font 18 (V4 0 255 0 255) (T.pack $ map chr [0..127]))
                (G.deleteTextTexture . V.toList)
    liftIO $ do
      initOthers
      env <- mkEnv assetManager win r glContext font ascii
      let kst = KurokosState
            { kstMessages = []
            , kstSdlEvents = []
            , kstSceneState = SceneState 0
            , kstJoysticks = V.empty
            , kstStart = 0
            , kstCount = 0
            --
            , kstActualFps = 0
            }
      go $ KurokosData (env, kst)
  where
    withSDL = E.bracket_ SDL.initializeAll SDL.quit

    initOthers =
      SDL.setMouseLocationMode SDL.RelativeLocation

    mkEnv assetManager win r glContext font ascii = do
      mvar <- MVar.newMVar r
      return KurokosEnv
        { envGraphFps = 60
        , envWindow = win
        , envMRenderer = mvar
        , envGLContext = glContext
        , envSystemAssets = assetManager
        , envSystemFont = font
        , envAsciiTextures = ascii
        , envDebugPrintFps = confDebugPrintFPS
        , envDebugPrintSystem = confDebugPrintSystem
        }

    withWinRenderer :: ((SDL.Window, G.Renderer, SDL.GLContext) -> IO r) -> IO r
    withWinRenderer act =
      withW $ \win ->
        withGLContext win $ \goContext -> do -- Initialize for OpenGL
          initOpenGL win
          withR win $ \renderer ->
            act (win, renderer, goContext)
      where
        withW =
          E.bracket (SDL.createWindow confWinTitle winConf)
                    SDL.destroyWindow

        withGLContext win =
          E.bracket (SDL.glCreateContext win)
                    SDL.glDeleteContext

        initOpenGL win = do
          V2 winW winH <- get $ SDL.windowSize win
          GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral winW) (fromIntegral winH))
          SDL.swapInterval $= SDL.SynchronizedUpdates
          GL.clearColor $= GL.Color4 0 0 0 1

        withR win f = do
          size <- get $ SDL.windowSize win
          E.bracket (G.newRenderer size) G.freeRenderer f

-- * Scene

-- | Scene function type for updating `g`.
type Update m a = a -> KurokosT m a
-- | Scene function type for rendering `g`.
type Render m a = a -> KurokosT m ()
-- | Scene function type for choosing scenes for next frame.
type Transit m a b = a -> KurokosT m (Transition a b)

data Scene a m b = Scene
  { sceneUpdate  :: Update m a
  , sceneRender  :: Render m a
  , sceneTransit :: Transit m a b
  }

newtype SceneState = SceneState
  { frameCount  :: Integer
  }

data Transition a b
  = Continue a
  | End b

-- | Equivalent to `return . Continue`
continue :: Monad m => a -> m (Transition a b)
continue = return . Continue

-- | Equivalent to `return . End`
end :: Monad m => b -> m (Transition a b)
end = return . End

runScene :: (MonadBaseControl IO m, MonadIO m) => Scene a m b -> a -> KurokosT m b
runScene Scene{..} =
  loop (SceneState 0)
  where
    loop sst0 a0 = do
      updateTime
      -- Update
      events <- SDL.pollEvents
      procEvents events
      modify' $ \kst -> kst { kstSdlEvents = events
                            , kstSceneState = sst0 } -- For getFrame
      a1 <- sceneUpdate a0
      -- Rendering
      preRender
      sceneRender a1
      -- updateFPS
      printSystemState
      printMessages
      SDL.glSwapWindow =<< asks envWindow
      -- Transition
      trans <- sceneTransit a1
      -- Advance State
      wait
      let sst1 = advance sst0
      -- Go to next loop
      case trans of
        Continue a2 -> loop sst1 a2
        End b       -> return b

    -- TODO: Implement frame skip
    updateTime :: MonadIO m => KurokosT m ()
    updateTime = do
      cnt <- gets kstCount
      t0 <- gets kstStart
      t <- SDL.ticks
      if | cnt == 0  -> modify' $ \a -> a {kstStart = t}
         | cnt == 60 -> do
              let fps = (60 * 1000) / fromIntegral (t - t0)
              modify' $ \a -> a {kstCount = 0, kstStart = t, kstActualFps = fps}
         | otherwise -> return ()
      modify' $ \kst -> kst {kstCount = kstCount kst + 1}

    wait :: MonadIO m => KurokosT m ()
    wait = do
      t <- SDL.ticks
      cnt <- gets kstCount
      t0 <- gets kstStart
      fps <- asks envGraphFps
      let lapse = t - t0
          tWait = fromIntegral (cnt * 1000) / fromIntegral fps - fromIntegral lapse
          tWait' = truncate (tWait :: Double)
      when (tWait > 0) $ SDL.delay tWait'

      -- Print meter
      p <- asks envDebugPrintSystem
      when p $ printDebug $
        if tWait > 0
          then
            let n = fromIntegral tWait'
            in printf "%02d" n ++ " " ++ replicate n '.'
          else "No Wait"

    preRender :: MonadIO m => KurokosT m ()
    preRender = liftIO $ do
      GL.clearColor $= GL.Color4 0 0 0 1
      GL.clear [GL.ColorBuffer]

    printSystemState :: MonadIO m => KurokosT m ()
    printSystemState = do
      p2 <- asks envDebugPrintFps
      when p2 $ do
        fps <- truncate <$> gets kstActualFps
        printDebug . show $ (fps :: Int)

    advance :: SceneState -> SceneState
    advance s = s {frameCount = c + 1}
      where
        c = frameCount s

    printMessages :: MonadIO m => KurokosT m ()
    printMessages = do
      ts <- asks envAsciiTextures
      msgs <- gets kstMessages
      modify $ \s -> s {kstMessages = []} -- Clear kstMessages
      withRenderer $ \r ->
        foldM_ (work r ts) 30 msgs
      where
        work r ts y cs = do
          G.renderText r pos $ mapMaybe (\c -> ts V.!? ord c) cs
          return $ y + 20
          where
            pos = V2 8 y

printDebug :: Monad m => String -> KurokosT m ()
printDebug str
  | null str  = return ()
  | otherwise = modify $ \s -> s {kstMessages = str : kstMessages s}

-- | Process events about system
procEvents :: MonadIO m => [SDL.Event] -> KurokosT m ()
procEvents = mapM_ (work . SDL.eventPayload)
  where
    work :: MonadIO m => SDL.EventPayload -> KurokosT m ()
    work (SDL.WindowClosedEvent _) = liftIO $ E.throwIO UserExitException
    work SDL.QuitEvent             = liftIO $ E.throwIO UserExitException
    work (SDL.JoyDeviceEvent dt)   = resetJoysticks dt
    work _                         = return ()

-- TODO: Should check joyDeviceEventConnction
resetJoysticks :: MonadIO m => SDL.JoyDeviceEventData -> KurokosT m ()
resetJoysticks SDL.JoyDeviceEventData{} = do
  vjs <- getJoysticks
  V.mapM_ closeJoystick vjs
  vjs' <- liftIO (V.mapM openJoystickFromDevice =<< SDL.availableJoysticks)
  modify' $ \kst -> kst {kstJoysticks = vjs'}

--

-- getEnv :: (MonadReader KurokosEnv m) => m KurokosEnv
-- getEnv = ask

getEvents :: Monad m => KurokosT m [SDL.EventPayload]
getEvents = map SDL.eventPayload <$> gets kstSdlEvents

getFrame :: Monad m => KurokosT m Integer
getFrame = frameCount <$> gets kstSceneState

getJoysticks :: Monad m => KurokosT m (V.Vector Joystick)
getJoysticks = gets kstJoysticks

showMessageBox :: (MonadReader KurokosEnv m, MonadIO m) => Text -> Text -> m ()
showMessageBox title message = do
  win <- Just <$> asks envWindow
  SDL.showSimpleMessageBox win SDL.Information title message

--

instance (MonadReader KurokosEnv m, MonadIO m) => RenderEnv m where
  getWindow = asks envWindow

  getWindowSize = SDL.get . SDL.windowSize =<< getWindow

  getRenderer = liftIO . MVar.readMVar =<< asks envMRenderer

  withRenderer act = do
    mvar <- asks envMRenderer
    liftIO $
      E.bracket (MVar.takeMVar mvar)
                (MVar.putMVar mvar)
                act

  -- renderTexture tex pos size =
  --   withRenderer $ \r ->
  --     G.renderTexture_ r tex rctx
  --   where
  --     rctx = G.RContext pos size Nothing Nothing
