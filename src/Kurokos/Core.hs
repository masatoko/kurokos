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
  , KurokosEnvT
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
  , runKurokosEnvT
  , withKurokos
  --
  , printsys
  , getEnv
  , screenSize
  , getWindow
  , getEvents
  , getJoysticks
  , averageTime
  , showMessageBox
  , getRenderer
  , withRenderer
  , setRendererDrawBlendMode
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
import qualified Data.Vector.Unboxed          as VU
import           Data.Word                    (Word32)
import           Foreign.C.Types              (CInt)
import           Linear.Affine                (Point (..))
import           Linear.V2
import           Linear.V4
import           Text.Printf                  (printf)

import           SDL                          (($=))
import qualified SDL
import qualified SDL.Font                     as Font

import           Kurokos.Metapad
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

type Time = Word32

data KurokosEnv = KurokosEnv
  { graphFPS         :: Int
  , window           :: SDL.Window
  -- Resource
  , renderer         :: MVar SDL.Renderer
  , systemAssets     :: Asset.SDLAssetManager
  , systemFont       :: Font.Font
  -- Debug
  , debugPrintFPS    :: Bool
  , debugPrintSystem :: Bool
  }

data KurokosState = KurokosState
  {
    messages      :: [Text]
  , kstEvents     :: [SDL.Event]
  , kstJoysticks  :: V.Vector Joystick
  --
  , psStart       :: !Time
  , psCount       :: !Int
  --
  , actualFPS     :: !Double
  , frameTimes    :: VU.Vector Time
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

newtype KurokosEnvT a = KurokosEnvT {
    runKET :: ReaderT KurokosEnv IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader KurokosEnv, MonadThrow, MonadCatch, MonadMask)

runKurokosEnvT :: KurokosEnv -> KurokosEnvT a -> IO a
runKurokosEnvT conf k = runReaderT (runKET k) conf

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
            { messages = []
            , kstEvents = []
            , kstJoysticks = V.empty
            , psStart = 0
            , psCount = 0
            --
            , actualFPS = 0
            , frameTimes = VU.empty
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
        { graphFPS = 60
        , window = win
        , renderer = mvar
        , systemAssets = sdlAssetManager
        , systemFont = font
        , debugPrintFPS = confDebugPrintFPS
        , debugPrintSystem = confDebugPrintSystem
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

-- Scene
type Update g m a  = SceneState -> [a] -> g -> KurokosT m g
type Render g m    = SceneState -> g -> KurokosT m ()
type Transit g m a = SceneState -> [a] -> g -> KurokosT m (Maybe (Transition m))

data Scene g m a = Scene
  { scenePad     :: Metapad a
  , sceneUpdate  :: Update g m a
  , sceneRender  :: Render g m
  , sceneTransit :: Transit g m a
  , sceneAlloca  :: ResourceT (KurokosT m) g
  }

newtype SceneState = SceneState
  { frameCount  :: Integer
  }

type Exec m = KurokosT m ()

data Transition m
  = End
  | forall g a. Next (Scene g m a)
  | forall g a. Push (Scene g m a)

continue :: Monad m => m (Maybe (Transition base))
continue = return Nothing

end :: Monad m => m (Maybe (Transition base))
end = return $ Just End

next :: Monad m => Scene g m a -> KurokosT m (Maybe (Transition m))
next = return . Just . Next

push :: Monad m => Scene g m a -> KurokosT m (Maybe (Transition m))
push = return . Just . Push

-- Start scene
runScene :: (MonadBaseControl IO m, MonadMask m, MonadIO m) => Scene g m a -> KurokosT m ()
runScene scn0 =
  goScene scn0 >>= \case
    Nothing   -> return ()
    Just exec -> exec

goScene :: (MonadBaseControl IO m, MonadMask m, MonadIO m) => Scene g m a -> KurokosT m (Maybe (Exec m))
goScene scene_ =
  runResourceT $ do
    g <- sceneAlloca scene_
    lift $ go (SceneState 0) scene_ g
  where
    go :: (MonadBaseControl IO m, MonadMask m, MonadIO m) => SceneState -> Scene g m a -> g -> KurokosT m (Maybe (Exec m))
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

sceneLoop :: (MonadIO m, MonadMask m) => g -> SceneState -> Scene g m a -> KurokosT m (g, SceneState, Transition m)
sceneLoop iniG iniS scene =
  loop Nothing iniG iniS
  where
    pad = scenePad scene
    update = sceneUpdate scene
    render = sceneRender scene
    transit = sceneTransit scene
    --
    loop mPreInput g s0 = do
      updateTime
      -- Update
      events <- SDL.pollEvents
      procEvents events
      (actions, curInput) <- makeActions mPreInput events pad
      modify' $ \kst -> kst {kstEvents = events}
      g' <- update s0 actions g
      -- Rendering
      preRender
      render s0 g'
      -- updateFPS
      printSystemState
      printMessages
      withRenderer SDL.present
      -- Transition
      mTrans <- transit s0 actions g'
      -- Advance State
      wait
      let s1 = advance s0
      -- Go next loop
      shouldExit <- gets kstShouldExit
      if shouldExit
        then return (g', s1, End)
        else
          case mTrans of
            Nothing    -> loop (Just curInput) g' s1
            Just trans -> return (g', s1, trans)

    -- TODO: Implement frame skip
    updateTime :: MonadIO m => KurokosT m ()
    updateTime = do
      cnt <- gets psCount
      t0 <- gets psStart
      t <- SDL.ticks
      if | cnt == 0  -> modify' $ \a -> a {psStart = t}
         | cnt == 60 -> do
              let fps = (60 * 1000) / fromIntegral (t - t0)
              modify' $ \a -> a {psCount = 0, psStart = t, actualFPS = fps}
         | otherwise -> return ()
      modify' $ \a -> a {psCount = psCount a + 1}

    wait :: MonadIO m => KurokosT m ()
    wait = do
      t <- SDL.ticks
      cnt <- gets psCount
      t0 <- gets psStart
      fps <- asks graphFPS
      let lapse = t - t0
          tWait = fromIntegral (cnt * 1000) / fromIntegral fps - fromIntegral lapse
          tWait' = truncate (tWait :: Double)
      when (tWait > 0) $ SDL.delay tWait'

      -- Print meter
      p <- asks debugPrintSystem
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
      p2 <- asks debugPrintFPS
      when p2 $ do
        fps <- truncate <$> gets actualFPS
        printsys . T.pack . show $ (fps :: Int)

    advance :: SceneState -> SceneState
    advance s = s {frameCount = c + 1}
      where c = frameCount s

    printMessages :: (MonadIO m, MonadMask m) => KurokosT m ()
    printMessages = do
      font <- asks systemFont
      ts <- gets messages
      modify $ \s -> s {messages = []} -- Clear messages
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
  | otherwise   = modify $ \s -> s {messages = text : messages s}

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

getEnv :: (MonadReader KurokosEnv m) => m KurokosEnv
getEnv = ask

screenSize :: (MonadReader KurokosEnv m, MonadIO m) => m (V2 CInt)
screenSize = SDL.get . SDL.windowSize =<< asks window

-- getWindow :: (MonadReader KurokosEnv m, MonadIO m) => m SDL.Window
-- getWindow = asks window

getEvents :: Monad m => KurokosT m [SDL.EventPayload]
getEvents = map SDL.eventPayload <$> gets kstEvents

getJoysticks :: Monad m => KurokosT m (V.Vector Joystick)
getJoysticks = gets kstJoysticks

averageTime :: Monad m => KurokosT m Int
averageTime = do
  ts <- gets frameTimes
  let a = fromIntegral $ VU.sum ts
      n = VU.length ts
  return $ if n == 0
             then 0
             else a `div` n

showMessageBox :: (MonadReader KurokosEnv m, MonadIO m) => Text -> Text -> m ()
showMessageBox title message = do
  win <- Just <$> asks window
  SDL.showSimpleMessageBox win SDL.Information title message

--

setRendererDrawBlendMode :: (MonadIO m, MonadMask m) => SDL.BlendMode -> KurokosT m ()
setRendererDrawBlendMode mode =
  withRenderer $ \r ->
    SDL.rendererDrawBlendMode r $= mode


--

instance (MonadReader KurokosEnv m, MonadIO m, MonadMask m) => RenderEnv m where
  getWindow = asks window

  getWindowSize = SDL.get . SDL.windowSize =<< getWindow

  getRenderer = liftIO . readMVar =<< asks renderer

  withRenderer act = do
    mvar <- asks renderer
    liftIO $
      bracket (takeMVar mvar)
              (putMVar mvar)
              act

  renderTexture tex rect =
    withRenderer $ \r ->
      SDL.copy r tex Nothing (Just rect)
