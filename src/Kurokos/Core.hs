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
  , Render
  , Scene (..)
  , SceneState (..)
  , Transit
  , Transition (..)
  , Update
  --
  , continue, end, next, push
  , runScene
  --
  , runKurokos
  , withKurokos
  --
  , printsys
  , getWindowSize
  , getWindow
  , getEvents
  , getJoysticks
  , showMessageBox
  , getRenderer
  , withRenderer
  ) where

import           Control.Concurrent.MVar      (MVar, newMVar, putMVar, readMVar,
                                               takeMVar)
import           Control.Exception            (bracket)
import           Control.Exception.Safe       (MonadCatch, MonadMask,
                                               MonadThrow)
import qualified Control.Exception.Safe       as E
import           Control.Monad.Base           (MonadBase)
import           Control.Monad.Managed        (managed, managed_, runManaged)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import           Data.Word                    (Word32)
import           Linear.Affine                (Point (..))
import           Linear.V2
import           Linear.V4
import           Text.Printf                  (printf)

import           SDL                          (($=))
import qualified SDL
import qualified SDL.Font                     as Font

import           Kurokos.Types

import qualified Kurokos.Asset                as Asset
import qualified Kurokos.Asset.SDL            as Asset
import           Kurokos.UI                   (RenderEnv (..))

data KurokosConfig = KurokosConfig
  { confWinTitle         :: Text
  -- ^ Tile on the title bar of the window
  , confDebugPrintFPS    :: Bool
  -- ^ Print FPS for debugging
  , confDebugPrintSystem :: Bool
  -- ^ Print system information for debugging
  , confSystemAsset      :: Asset.AssetManager
  , confSystemFontId     :: Asset.Ident
  }

type Frame = Word32

data KurokosEnv = KurokosEnv
  { envGraphFps         :: Int
  , envWindow           :: SDL.Window
  -- Resource
  , envMRenderer        :: MVar SDL.Renderer
  , envSystemAssets     :: Asset.SDLAssetManager
  , envSystemFont       :: Font.Font
  -- Debug
  , envDebugPrintFps    :: Bool
  , envDebugPrintSystem :: Bool
  }

data KurokosState = KurokosState
  {
    kstMessages   :: [Text]
  , kstSdlEvents  :: [SDL.Event]
  , kstJoysticks  :: V.Vector Joystick
  --
  , kstStart      :: !Frame
  , kstCount      :: !Int
  --
  , kstActualFps  :: !Double
  , kstShouldExit :: Bool
  }

newtype KurokosData = KurokosData (KurokosEnv, KurokosState)

newtype KurokosT m a = KurokosT {
    runKT :: ReaderT KurokosEnv (StateT KurokosState m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader KurokosEnv, MonadState KurokosState, MonadThrow, MonadCatch, MonadMask, MonadBase base)

runKurokos :: KurokosData -> KurokosT m a -> m (a, KurokosState)
runKurokos (KurokosData (conf, stt)) k = runStateT (runReaderT (runKT k) conf) stt

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
    managed_ withFontInit
    (win, r) <- managed withWinRenderer
    sdlAssetManager <- managed $ E.bracket (Asset.newSDLAssetManager r confSystemAsset)
                                           Asset.freeSDLAssetManager
    liftIO $ do
      initOthers r
      env <- mkEnv sdlAssetManager win r
      let kst = KurokosState
            { kstMessages = []
            , kstSdlEvents = []
            , kstJoysticks = V.empty
            , kstStart = 0
            , kstCount = 0
            --
            , kstActualFps = 0
            , kstShouldExit = False
            }
      go $ KurokosData (env, kst)
  where
    withSDL = E.bracket_ SDL.initializeAll SDL.quit

    initOthers r = do
      _ <- SDL.setMouseLocationMode SDL.RelativeLocation
      SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend

    withFontInit = E.bracket_ Font.initialize Font.quit

    mkEnv sdlAssetManager win r = do
      mvar <- newMVar r
      font <- Asset.getFont confSystemFontId 14 sdlAssetManager
      return KurokosEnv
        { envGraphFps = 60
        , envWindow = win
        , envMRenderer = mvar
        , envSystemAssets = sdlAssetManager
        , envSystemFont = font
        , envDebugPrintFps = confDebugPrintFPS
        , envDebugPrintSystem = confDebugPrintSystem
        }

    withWinRenderer :: ((SDL.Window, SDL.Renderer) -> IO r) -> IO r
    withWinRenderer act = withW $ withR act
      where
        withW = E.bracket (SDL.createWindow confWinTitle winConf)
                          SDL.destroyWindow

        withR func win = E.bracket (SDL.createRenderer win (-1) SDL.defaultRenderer)
                                   SDL.destroyRenderer
                                   (\r -> setLogicalSize r >> func (win,r))

        setLogicalSize r =
          case SDL.windowMode winConf of
            SDL.FullscreenDesktop -> work
            SDL.Fullscreen        -> work
            _                     -> return ()
          where
            work = do
              let size = Just $ SDL.windowInitialSize winConf
              SDL.rendererLogicalSize r $= size

-- * Scene

-- | Scene function type for updating `g`.
type Update m g  = SceneState -> g -> KurokosT m g
-- | Scene function type for rendering `g`.
type Render m g  = SceneState -> g -> KurokosT m ()
-- | Scene function type for choosing scenes for next frame.
type Transit m g = SceneState -> g -> KurokosT m (Maybe (Transition m))

data Scene m g = Scene
  { sceneUpdate  :: Update m g
  , sceneRender  :: Render m g
  , sceneTransit :: Transit m g
  , sceneAlloca  :: ResourceT (KurokosT m) g
  }

newtype SceneState = SceneState
  { frameCount  :: Integer
  }

type Exec m = KurokosT m ()

data Transition m
  = End
  | forall g. Next (Scene m g)
  | forall g. Push (Scene m g)

-- | Equivalent to `return Nothing`
continue :: Monad m => m (Maybe (Transition base))
continue = return Nothing

-- | Transit to next scene.
next :: Monad m => Scene m g -> KurokosT m (Maybe (Transition m))
next = return . Just . Next

-- | Abort this scene and transit to next scene.
push :: Monad m => Scene m g -> KurokosT m (Maybe (Transition m))
push = return . Just . Push

-- | Finish this scene
-- Equivalent to `return $ Just End`
end :: Monad m => m (Maybe (Transition base))
end = return $ Just End


-- Start scene
runScene :: (MonadBaseControl IO m, MonadMask m, MonadIO m) => Scene m g -> KurokosT m ()
runScene scn0 =
  goScene scn0 >>= \case
    Nothing   -> return ()
    Just exec -> exec

goScene :: (MonadBaseControl IO m, MonadMask m, MonadIO m) => Scene m g -> KurokosT m (Maybe (Exec m))
goScene scene_ =
  runResourceT $ do
    g <- sceneAlloca scene_
    lift $ go (SceneState 0) scene_ g
  where
    go :: (MonadBaseControl IO m, MonadMask m, MonadIO m) => SceneState -> Scene m g -> g -> KurokosT m (Maybe (Exec m))
    go s0 scene0 g0 = do
      (g', s', trans) <- sceneLoop g0 s0 scene0
      case trans of
        End       -> return Nothing
        Next s -> return $ Just (runScene s)
        Push s -> do
          mExec <- goScene s
          case mExec of
            Just _  -> return mExec
            Nothing -> go s' scene0 g'

sceneLoop :: (MonadIO m, MonadMask m) => g -> SceneState -> Scene m g -> KurokosT m (g, SceneState, Transition m)
sceneLoop iniG iniS Scene{..} =
  loop iniG iniS
  where
    loop g s0 = do
      updateTime
      -- Update
      events <- SDL.pollEvents
      procEvents events
      modify' $ \kst -> kst {kstSdlEvents = events}
      g' <- sceneUpdate s0 g
      -- Rendering
      preRender
      sceneRender s0 g'
      -- updateFPS
      printSystemState
      printMessages
      withRenderer SDL.present
      -- Transition
      mTrans <- sceneTransit s0 g'
      -- Advance State
      wait
      let s1 = advance s0
      -- Go next loop
      shouldExit <- gets kstShouldExit
      if shouldExit
        then return (g', s1, End)
        else
          case mTrans of
            Nothing    -> loop g' s1
            Just trans -> return (g', s1, trans)

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
      when p $ printsys . T.pack $
        if tWait > 0
          then
            let n = fromIntegral tWait'
            in printf "%02d" n ++ " " ++ replicate n '.'
          else "NO WAIT"

    preRender :: (MonadIO m, MonadMask m) => KurokosT m ()
    preRender =
      withRenderer $ \r -> do
        SDL.rendererDrawColor r $= V4 0 0 0 255
        SDL.clear r

    printSystemState :: MonadIO m => KurokosT m ()
    printSystemState = do
      p2 <- asks envDebugPrintFps
      when p2 $ do
        fps <- truncate <$> gets kstActualFps
        printsys . T.pack . show $ (fps :: Int)

    advance :: SceneState -> SceneState
    advance s = s {frameCount = c + 1}
      where c = frameCount s

    printMessages :: (MonadIO m, MonadMask m) => KurokosT m ()
    printMessages = do
      font <- asks envSystemFont
      ts <- gets kstMessages
      modify $ \s -> s {kstMessages = []} -- Clear kstMessages
      withRenderer $ \r ->
        foldM_ (work r font) 8 ts
      where
        work r font y text = liftIO $ do
          (w,h) <- Font.size font text
          runManaged $ do
            surface <- managed $ E.bracket (Font.blended font (V4 0 255 0 255) text) SDL.freeSurface
            texture <- managed $ E.bracket (SDL.createTextureFromSurface r surface) SDL.destroyTexture
            let rect = Just $ SDL.Rectangle (P (V2 8 y)) (fromIntegral <$> V2 w h)
            SDL.copy r texture Nothing rect
          return $ y + fromIntegral h

printsys :: Monad m => Text -> KurokosT m ()
printsys text
  | T.null text = return ()
  | otherwise   = modify $ \s -> s {kstMessages = text : kstMessages s}

-- | Process events about system
procEvents :: MonadIO m => [SDL.Event] -> KurokosT m ()
procEvents = mapM_ (work . SDL.eventPayload)
  where
    work :: MonadIO m => SDL.EventPayload -> KurokosT m ()
    work (SDL.WindowClosedEvent _) = modify' $ \kst -> kst {kstShouldExit = True}
    work SDL.QuitEvent             = modify' $ \kst -> kst {kstShouldExit = True}
    work (SDL.JoyDeviceEvent dt)   = resetJoysticks dt
    work _ = return ()

-- TODO: Shold check joyDeviceEventConnction
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

getJoysticks :: Monad m => KurokosT m (V.Vector Joystick)
getJoysticks = gets kstJoysticks

showMessageBox :: (MonadReader KurokosEnv m, MonadIO m) => Text -> Text -> m ()
showMessageBox title message = do
  win <- Just <$> asks envWindow
  SDL.showSimpleMessageBox win SDL.Information title message

--

instance (MonadReader KurokosEnv m, MonadIO m, MonadMask m) => RenderEnv m where
  getWindow = asks envWindow

  getWindowSize = SDL.get . SDL.windowSize =<< getWindow

  getRenderer = liftIO . readMVar =<< asks envMRenderer

  withRenderer act = do
    mvar <- asks envMRenderer
    liftIO $
      bracket (takeMVar mvar)
              (putMVar mvar)
              act

  renderTexture tex rect =
    withRenderer $ \r ->
      SDL.copy r tex Nothing (Just rect)
