{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Kurokos.Core
  ( Config (..)
  , defaultConfig
  , KurokosEnv (..)
  , DebugJoystick (..)
  , KurokosData
  , KurokosT
  , KurokosEnvT
  , Render
  , Scene (..)
  , SceneState (..)
  , Transit
  , Transition
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
  , averageTime
  , showMessageBox
  , withRenderer
  , setRendererDrawBlendMode
  ) where

import           Control.Concurrent.MVar     (MVar, newMVar, withMVar)
import           Control.Exception.Safe      (MonadCatch, MonadMask, MonadThrow)
import qualified Control.Exception.Safe      as E
import           Control.Monad.Base
import           Control.Monad.Managed       (managed, runManaged)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import qualified Data.ByteString             as B
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Vector.Unboxed         as V
import           Data.Word                   (Word32)
import           Linear.Affine               (Point (..))
import           Linear.V2
import           Linear.V4
import           System.Directory            (doesFileExist)
import           System.Exit                 (exitSuccess)
import           Text.Printf                 (printf)

import           SDL                         (($=))
import qualified SDL
import qualified SDL.Font                    as Font

import           Kurokos.Data                (Font)
import           Kurokos.Font                (freeFont, loadFont, withFont)
import           Kurokos.Metapad

data Config = Config
  { confWinSize          :: V2 Int
  , confWinTitle         :: String
  , confWindowMode       :: SDL.WindowMode
  , confDebugPrintFPS    :: Bool
  , confDebugPrintSystem :: Bool
  , confDebugJoystick    :: DebugJoystick
  , confNumAverageTime   :: Int
  , confFont             :: Either B.ByteString FilePath
  }

data DebugJoystick = DebugJoystick
  { djVisButton :: Bool
  , djVisAxis   :: Bool
  , djVisHat    :: Bool
  }

defaultConfig :: Config
defaultConfig = Config
  { confWinSize = V2 640 480
  , confWinTitle = "Kurokos"
  , confWindowMode = SDL.Windowed
  , confDebugPrintFPS = False
  , confDebugPrintSystem = False
  , confDebugJoystick = DebugJoystick False False False
  , confNumAverageTime = 60
  , confFont = Right "data/font/system.ttf"
  }

type Time = Word32

data KurokosEnv = KurokosEnv
  { graphFPS         :: Int
  , scrSize          :: V2 Int
  , window           :: SDL.Window
  -- Resource
  , renderer         :: MVar SDL.Renderer
  , systemFont       :: Font
  -- Debug
  , debugPrintFPS    :: Bool
  , debugPrintSystem :: Bool
  , debugJoystick    :: DebugJoystick
  , numAverateTime   :: Int
  }

data KurokosState = KurokosState
  {
    messages   :: [Text]
  --
  , psStart    :: !Time
  , psCount    :: !Int
  --
  , actualFPS  :: !Double
  , frameTimes :: V.Vector Time
  }

data KurokosData = KurokosData KurokosEnv KurokosState

initialState :: KurokosState
initialState = KurokosState
  {
    messages = []
  , psStart = 0
  , psCount = 0
  --
  , actualFPS = 0
  , frameTimes = V.empty
  }

newtype KurokosT m a = KurokosT {
    runKT :: ReaderT KurokosEnv (StateT KurokosState m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader KurokosEnv, MonadState KurokosState, MonadThrow, MonadCatch, MonadMask, MonadBase base)

runKurokos :: KurokosData -> KurokosT m a -> m (a, KurokosState)
runKurokos (KurokosData conf stt) k = runStateT (runReaderT (runKT k) conf) stt

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

withKurokos :: Config -> (KurokosData -> IO ()) -> IO ()
withKurokos config go =
  E.bracket_ SDL.initializeAll SDL.quit $ do
    specialInit
    withFontInit $ withFont' $ \font ->
      withWinRenderer config $ \win r -> do
        SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend
        conf <- mkConf font win r
        go $ KurokosData conf initialState
  where
    specialInit = do
      _ <- SDL.setMouseLocationMode SDL.RelativeLocation
      return ()

    withFontInit action =
      E.bracket_ Font.initialize
                 Font.quit
                 action

    withFont' action =
      case confFont config of
        Left bytes -> withFont bytes size action
        Right path -> E.bracket (loadFont path size)
                                freeFont
                                action
      where
        size = max 18 (h `div` 50)
        V2 _ h = confWinSize config

    mkConf font win r = do
      mvar <- newMVar r
      return KurokosEnv
        { graphFPS = 60
        , scrSize = confWinSize config
        , window = win
        , renderer = mvar
        , systemFont = font
        , debugPrintFPS = confDebugPrintFPS config
        , debugPrintSystem = confDebugPrintSystem config
        , debugJoystick = confDebugJoystick config
        , numAverateTime = confNumAverageTime config
        }

    withWinRenderer :: Config -> (SDL.Window -> SDL.Renderer -> IO a) -> IO a
    withWinRenderer conf work = withW $ withR work
      where
        title = T.pack $ confWinTitle conf

        withW = E.bracket (SDL.createWindow title winConf)
                          SDL.destroyWindow

        withR func win = E.bracket (SDL.createRenderer win (-1) SDL.defaultRenderer)
                                   SDL.destroyRenderer
                                   (\r -> setLogicalSize r >> func win r)

        winConf = SDL.defaultWindow
          { SDL.windowMode = confWindowMode conf
          , SDL.windowResizable = False
          , SDL.windowInitialSize = fromIntegral <$> confWinSize conf
          }

        setLogicalSize r =
          case confWindowMode conf of
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

data SceneState = SceneState
  { frameCount  :: Integer
  , sceneEvents :: [SDL.Event]
  }

-- type SceneStarter g a = (Scene g a, KurokosT g, g -> KurokosT m ())

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
    lift $ go (SceneState 0 []) scene_ g
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

sceneLoop :: MonadIO m => g -> SceneState -> Scene g m a -> KurokosT m (g, SceneState, Transition m)
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
      let s1 = s0 {sceneEvents = events}
      g' <- update s1 actions g
      -- Rendering
      preRender
      render s1 g'
      -- updateFPS
      printSystemState s1
      printMessages
      withRenderer SDL.present
      -- Transition
      mTrans <- transit s1 actions g'
      -- Advance State
      wait
      let s2 = advance s1
      -- Go next loop
      case mTrans of
        Nothing    -> loop (Just curInput) g' s2
        Just trans -> return (g', s2, trans)

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

    preRender :: MonadIO m => KurokosT m ()
    preRender =
      withRenderer $ \r -> do
        SDL.rendererDrawColor r $= V4 0 0 0 255
        SDL.clear r

    printSystemState :: MonadIO m => SceneState -> KurokosT m ()
    printSystemState stt = do
      -- p1 <- asks debugPrintSystem
      -- when p1 $
      --   printsys . T.pack . show . frameCount $ stt

      p2 <- asks debugPrintFPS
      when p2 $
        printsys =<< (T.pack . show . truncate <$> gets actualFPS)

    advance :: SceneState -> SceneState
    advance s = s {frameCount = c + 1}
      where c = frameCount s

    printMessages :: MonadIO m => KurokosT m ()
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
procEvents es = go =<< asks debugJoystick
  where
    go dj = mapM_ (work . SDL.eventPayload) es
      where
        work :: MonadIO m => SDL.EventPayload -> KurokosT m ()
        work (SDL.WindowClosedEvent _) = liftIO exitSuccess
        work SDL.QuitEvent             = liftIO exitSuccess
        work (SDL.JoyButtonEvent d)    =
          when (djVisButton dj) $ liftIO . print $ d
        work (SDL.JoyAxisEvent d)      =
          when (djVisAxis dj) $ liftIO . putStrLn . showJoyAxisEventData $ d
        work (SDL.JoyHatEvent d)       =
          when (djVisHat dj) $ liftIO . putStrLn . showJoyHatEventData $ d
        work _ = return ()

    showJoyAxisEventData (SDL.JoyAxisEventData jid axis value) =
      "Axis: " ++ show jid ++ " @ " ++ show axis ++ " - " ++ show value

    showJoyHatEventData (SDL.JoyHatEventData jid hat value) =
      "Hat: " ++ show jid ++ " @ " ++ show hat ++ " - " ++ show value

--

getEnv :: (MonadReader KurokosEnv m, MonadIO m) => m KurokosEnv
getEnv = ask

screenSize :: (MonadReader KurokosEnv m, MonadIO m) => m (V2 Int)
screenSize = asks scrSize

getWindow :: (MonadReader KurokosEnv m, MonadIO m) => m SDL.Window
getWindow = asks window

averageTime :: Monad m => KurokosT m Int
averageTime = do
  ts <- gets frameTimes
  let a = fromIntegral $ V.sum ts
      n = V.length ts
  return $ if n == 0
             then 0
             else a `div` n

showMessageBox :: (MonadReader KurokosEnv m, MonadIO m) => Text -> Text -> m ()
showMessageBox title message = do
  window <- Just <$> asks window
  SDL.showSimpleMessageBox window SDL.Information title message

withRenderer :: (MonadReader KurokosEnv m, MonadIO m) => (SDL.Renderer -> IO a) -> m a
withRenderer act = do
  mvar <- asks renderer
  liftIO $ withMVar mvar act

--

setRendererDrawBlendMode :: MonadIO m => SDL.BlendMode -> KurokosT m ()
setRendererDrawBlendMode mode =
  withRenderer $ \r ->
    SDL.rendererDrawBlendMode r $= mode
