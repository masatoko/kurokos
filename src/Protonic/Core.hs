{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}

module Protonic.Core
  ( Config (..)
  , defaultConfig
  , ProtoConfig (..)
  , DebugJoystick (..)
  , Proto
  , ProtoT
  , ProtoConfT
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
  , runProtoT
  , runProtoConfT
  , withProtonic
  --
  , printsys
  , getProtoConfig
  , screenSize
  , getWindow
  , averageTime
  , showMessageBox
  , withRenderer
  , setRendererDrawBlendMode
  ) where

import           Control.Concurrent.MVar (MVar, newMVar, withMVar)
import qualified Control.Exception.Safe  as E
import           Control.Exception.Safe  (MonadThrow, MonadCatch, MonadMask)
import           Control.Monad.Managed   (managed, runManaged)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString         as B
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector.Unboxed     as V
import           Data.Word               (Word32)
import           Linear.Affine           (Point (..))
import           Linear.V2
import           Linear.V4
import           System.Exit             (exitSuccess)
import           System.Directory        (doesFileExist)
import           Text.Printf             (printf)

import qualified SDL.Font                as Font
-- import qualified SDL.TTF                 as TTF
-- import           SDL.TTF.FFI             (TTFFont)
import           SDL                     (($=))
import qualified SDL

import           Protonic.Metapad
import           Protonic.Data           (Font (..))
import           Protonic.Font_          (newFont, freeFont, withFont)

data Config = Config
  { confWinSize :: V2 Int
  , confWinTitle :: String
  , confWindowMode :: SDL.WindowMode
  , confDebugPrintFPS :: Bool
  , confDebugPrintSystem :: Bool
  , confDebugJoystick :: DebugJoystick
  , confNumAverageTime :: Int
  , confFont :: Either B.ByteString FilePath
  }

data DebugJoystick = DebugJoystick
  { djVisButton :: Bool
  , djVisAxis :: Bool
  , djVisHat :: Bool
  }

defaultConfig :: Config
defaultConfig = Config
  { confWinSize = V2 640 480
  , confWinTitle = "protonic"
  , confWindowMode = SDL.Windowed
  , confDebugPrintFPS = False
  , confDebugPrintSystem = False
  , confDebugJoystick = DebugJoystick False False False
  , confNumAverageTime = 60
  , confFont = Right "data/font/system.ttf"
  }

type Time = Word32

data ProtoConfig = ProtoConfig
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

data ProtoState = ProtoState
  {
    messages     :: [Text]
  --
  , psStart      :: !Time
  , psCount      :: !Int
  --
  , actualFPS    :: !Double
  , frameTimes   :: V.Vector Time
  , execScene    :: Maybe (ProtoT ())
  }

data Proto = Proto ProtoConfig ProtoState

initialState :: ProtoState
initialState = ProtoState
  {
    messages = []
  , psStart = 0
  , psCount = 0
  --
  , actualFPS = 0
  , frameTimes = V.empty
  , execScene = Nothing
  }

newtype ProtoT a = ProtoT {
    runPT :: ReaderT ProtoConfig (StateT ProtoState IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ProtoConfig, MonadState ProtoState, MonadThrow, MonadCatch, MonadMask)

runProtoT :: Proto -> ProtoT a -> IO (a, ProtoState)
runProtoT (Proto conf stt) k = runStateT (runReaderT (runPT k) conf) stt

newtype ProtoConfT a = ProtoConfT {
    runPCT :: ReaderT ProtoConfig IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ProtoConfig, MonadThrow, MonadCatch, MonadMask)

runProtoConfT :: ProtoConfig -> ProtoConfT a -> IO a
runProtoConfT conf k = runReaderT (runPCT k) conf

withProtonic :: Config -> (Proto -> IO ()) -> IO ()
withProtonic config go =
  E.bracket_ SDL.initializeAll SDL.quit $ do
    specialInit
    withFontInit $ withFont' $ \font ->
      withWinRenderer config $ \win r -> do
        SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend
        conf <- mkConf font win r
        go $ Proto conf initialState
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
        Right path -> E.bracket (newFont path size)
                                freeFont
                                action
      where
        size = max 18 (h `div` 50)
        V2 _ h = confWinSize config

    mkConf font win r = do
      mvar <- newMVar r
      return ProtoConfig
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
            SDL.Fullscreen -> work
            _ -> return ()
          where
            work = do
              let size = Just $ SDL.windowInitialSize winConf
              SDL.rendererLogicalSize r $= size

-- Scene
type Update g a  = SceneState -> [a] -> g -> ProtoT g
type Render g    = SceneState -> g -> ProtoT ()
type Transit g a = SceneState -> [a] -> g -> ProtoT (Maybe Transition)

data Scene g a = Scene
  { scenePad     :: Metapad a
  , sceneUpdate  :: Update g a
  , sceneRender  :: Render g
  , sceneTransit :: Transit g a
  , sceneNew     :: ProtoT g
  , sceneDelete  :: g -> ProtoT ()
  }

data SceneState = SceneState
  { frameCount :: Integer
  , sceneEvents :: [SDL.Event]
  }

type SceneStarter g a = (Scene g a, ProtoT g, g -> ProtoT ())

type Exec = ProtoT ()

data Transition
  = End
  | Next Exec
  | Push Exec

continue :: Monad m => m (Maybe Transition)
continue = return Nothing

end :: Monad m => m (Maybe Transition)
end = return $ Just End

next :: Scene g a -> ProtoT (Maybe Transition)
next s = return . Just . Next $ runScene s

push :: Scene g a -> ProtoT (Maybe Transition)
push s = return . Just . Push $ goScene s

-- Start scene
runScene :: Scene g a -> ProtoT ()
runScene scn0 = do
  goScene scn0
  gets execScene >>= \case
    Nothing  -> return ()
    Just exec -> do
      modify' $ \pst -> pst {execScene = Nothing}
      exec

goScene :: Scene g a -> ProtoT ()
goScene scene_ =
  E.bracket (sceneNew scene_)
            (sceneDelete scene_)
            (go (SceneState 0 []) scene_)
  where
    go :: SceneState -> Scene g a -> g -> ProtoT ()
    go s0 scene0 g0 = do
      (g', s', trans) <- sceneLoop g0 s0 scene0
      case trans of
        End       -> return ()
        Next exec -> modify' $ \pst -> pst {execScene = Just exec}
        Push exec -> do
          exec
          gets execScene >>= \case
            Just _  -> return ()
            Nothing -> go s' scene0 g'

sceneLoop :: g -> SceneState -> Scene g a -> ProtoT (g, SceneState, Transition)
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
    updateTime :: ProtoT ()
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

    wait :: ProtoT ()
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

    preRender :: ProtoT ()
    preRender =
      withRenderer $ \r -> do
        SDL.rendererDrawColor r $= V4 0 0 0 255
        SDL.clear r

    printSystemState :: SceneState -> ProtoT ()
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

    printMessages :: ProtoT ()
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

printsys :: Text -> ProtoT ()
printsys text
  | T.null text = return ()
  | otherwise   = modify $ \s -> s {messages = text : messages s}

-- | Process events about system
procEvents :: [SDL.Event] -> ProtoT ()
procEvents es = go =<< asks debugJoystick
  where
    go dj = mapM_ (work . SDL.eventPayload) es
      where
        work :: SDL.EventPayload -> ProtoT ()
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

getProtoConfig :: (MonadReader ProtoConfig m, MonadIO m) => m ProtoConfig
getProtoConfig = ask

screenSize :: (MonadReader ProtoConfig m, MonadIO m) => m (V2 Int)
screenSize = asks scrSize

getWindow :: (MonadReader ProtoConfig m, MonadIO m) => m SDL.Window
getWindow = asks window

averageTime :: ProtoT Int
averageTime = do
  ts <- gets frameTimes
  let a = fromIntegral $ V.sum ts
      n = V.length ts
  return $ if n == 0
             then 0
             else a `div` n

showMessageBox :: (MonadReader ProtoConfig m, MonadIO m) => Text -> Text -> m ()
showMessageBox title message = do
  window <- Just <$> asks window
  SDL.showSimpleMessageBox window SDL.Information title message

withRenderer :: (MonadReader ProtoConfig m, MonadIO m) => (SDL.Renderer -> IO a) -> m a
withRenderer act = do
  mvar <- asks renderer
  liftIO $ withMVar mvar act

assert :: Bool -> IO ()
assert = flip unless $ error "Assertion failed"

--

setRendererDrawBlendMode :: SDL.BlendMode -> ProtoT ()
setRendererDrawBlendMode mode =
  withRenderer $ \r ->
    SDL.rendererDrawBlendMode r $= mode
