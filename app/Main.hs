{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State
import qualified Data.ByteString        as B
import           Data.Int               (Int16, Int32)
import qualified Data.Text              as T
import           Linear.Affine
import           Linear.V2
import           Linear.V4
import           Linear.Vector          ((^*))
import           System.Environment     (getArgs)
import           Control.Monad.Trans.Resource (ResourceT, allocate)

import qualified SDL
import qualified SDL.Primitive          as Prim

import           Kurokos                (Joystick, KurokosT, Metapad, Render,
                                         Scene (..), SceneState (..), Update,
                                         addAction, newPad, runKurokos,
                                         runScene, withKurokos)
import qualified Kurokos                as K

data Title = Title

data Game = Game
  { gSprite    :: K.Sprite
  , gImgSprite :: K.Sprite
  , gDeg       :: !Double
  , gCount     :: !Int
  , gActions   :: [Action]
  }

allocGame :: ResourceT (KurokosT IO) Game
allocGame = do
  liftIO . putStrLn $ "allocGame"
  env <- lift K.getEnv
  (_, font) <- allocate (K.loadFont fontPath 50)
                        K.freeFont
  (_, img) <- allocate (liftIO $ K.runKurokosEnvT env $ K.loadSprite "_data/img.png" (pure 48))
                       (\a -> K.freeSprite a >> liftIO (putStrLn "free img.png"))
  (_, char) <- allocate (liftIO $ K.runKurokosEnvT env $ K.newSprite font (V4 255 255 255 255) "@")
                        (\a -> K.freeSprite a >> liftIO (putStrLn "free font sprite"))
  -- lift $ K.withRenderer $ \r -> doSomething
  return $ Game char img 0 0 []
  where
    fontPath = "_data/system.ttf"

main :: IO ()
main = do
  as <- getArgs
  let opt = (`elem` as)
      conf = mkConf (opt "button") (opt "axis") (opt "hat")
      -- conf' = conf {K.confFont = Left fontBytes}
      conf' = conf {K.confFont = Right "_data/system.ttf"}
  withKurokos conf' $ \kuro -> do
    mjs <- K.newJoystickAt 0
    let gamepad = mkGamepad mjs
    _ <- runKurokos kuro $
      runScene $ titleScene mjs gamepad
    maybe (return ()) K.freeJoystick mjs
    return ()
  where
    mkConf pBtn pAxis pHat =
      K.defaultConfig
        { K.confWinSize = V2 640 480
        , K.confWinTitle = "protpnic-app"
        -- , K.confWindowMode = SDL.Fullscreen
        , K.confWindowMode = SDL.Windowed
        , K.confDebugPrintSystem = True
        , K.confDebugJoystick = K.DebugJoystick pBtn pAxis pHat
        }

    -- monitor mjs =
    --   case mjs of
    --     Nothing -> return ()
    --     Just js -> forever $ do
    --       clearScreen
    --       SDL.pumpEvents
    --       K.monitorJoystick js
    --       threadDelay 100000

data Action
  = Go
  | Enter
  | Exit
  | AxisLeft Int16 Int16
  | PUp | HUp | RUp
  | PDown
  --
  | MousePos (V2 Int)
  | MouseMotion (V2 Int32)
  | MouseWheel (V2 Int32)
  | TouchMotion (V2 Double)
  deriving (Eq, Show)

mkGamepad :: Maybe Joystick -> Metapad Action
mkGamepad mjs = flip execState newPad $ do
  -- Keyboard
  modify . addAction $ K.released SDL.ScancodeF Go
  modify . addAction $ K.pressed SDL.ScancodeReturn Enter
  modify . addAction $ K.pressed SDL.ScancodeEscape Exit
  -- Joystick
  case mjs of
    Just js -> do
      -- Buttons
      modify . addAction $ K.joyPressed js 4 Enter
      mapM_ (modify . addAction . uncurry (K.joyPressed js))
        [ (10, Go), (11, Go), (12, Go), (13, Go) ]
      -- Axes
      modify . addAction $ K.joyAxis2 js 0 1 AxisLeft
      -- Hat
      modify . addAction $ K.joyHat K.HDUp K.Pressed PUp
      modify . addAction $ K.joyHat K.HDUp K.Released RUp
      modify . addAction $ K.joyHat K.HDUp K.Holded HUp
      modify . addAction $ K.joyHat K.HDDown K.Pressed PDown
    Nothing -> return ()
  -- Mouse
  modify . addAction $ K.mouseButtonAct K.ButtonLeft K.Pressed Go
  modify . addAction $ K.mousePosAct MousePos
  modify . addAction $ K.mouseMotionAct MouseMotion
  modify . addAction $ K.mouseWheelAct MouseWheel
  -- Touch
  modify . addAction $ K.touchMotionAct TouchMotion

titleScene :: Maybe K.Joystick -> Metapad Action -> Scene Title IO Action
titleScene mjs pad =
  Scene pad update render transit (return Title) -- (\_ -> return ())
  where
    update :: Update Title IO Action
    update _ as t = return t

    render :: Render Title IO
    render _ _ = do
      K.printTest (P (V2 10 100)) (V4 0 255 255 255) "Enter - start"
      K.printTest (P (V2 10 120)) (V4 0 255 255 255) "Escape - exit"
      K.printTest (P (V2 10 160)) (V4 0 255 255 255) "日本語テスト"

    transit _ as _
      | Enter `elem` as = K.next $ mainScene mjs pad
      | Exit  `elem` as = K.end
      | otherwise       = K.continue

mainScene :: Maybe K.Joystick -> Metapad Action -> Scene Game IO Action
mainScene mjs pad = Scene pad update render transit allocGame
  where
    update :: Update Game IO Action
    update stt as g0 = do
      -- when (frameCount stt `mod` 60 == 0) $ K.averageTime >>= liftIO . print
      let alpha = fromIntegral $ frameCount stt
      K.setAlphaMod (gImgSprite g0) alpha
      execStateT go g0
      where
        go :: StateT Game (KurokosT IO) ()
        go = do
          mapM_ count as
          setDeg
          unless (null as) $ modify $ \g -> g {gActions = as}

        count :: Action -> StateT Game (KurokosT IO) ()
        count Go = do
          modify (\a -> let c = gCount a in a {gCount = c + 1})
          c <- gets gCount
          let strength = fromIntegral c * 0.2
              len = fromIntegral c * 100
          mapM_ (\joy -> K.rumble joy strength len) mjs
        count _  = return ()

        setDeg = modify (\g -> g {gDeg = fromIntegral (frameCount stt `mod` 360)})

    render :: Render Game IO
    render sst (Game spr img d i as) = do
      K.clearBy $ V4 0 0 0 255
      -- K.renderS spr (P (V2 150 200)) Nothing (Just d)
      -- K.renderS img (P (V2 10 200)) Nothing Nothing
      K.withRenderer $ \r -> do
        let p0 = V2 200 250
            p1 = p0 + (round <$> (V2 dx dy ^* 30))
              where
                dx = cos $ fromIntegral t / 5
                dy = sin $ fromIntegral t / 5
        Prim.thickLine r p0 p1 4 (V4 0 255 0 255)
      --
      K.printTest (P (V2 10 100)) color "Press Enter key to pause"
      K.printTest (P (V2 10 120)) color "Press F key!"
      let progress = replicate i '>' ++ replicate (targetCount - i) '-'
      K.printTest (P (V2 10 140)) color $ T.pack progress
      K.printTest (P (V2 10 160)) color $ T.pack $ show as
      where
        color = V4 255 255 255 255
        t = frameCount sst

    transit _ as g
      | cnt > targetCount = K.next $ clearScene mjs cnt pad
      | Enter `elem` as   = K.push $ pauseScene pad
      --
      | PUp   `elem` as   = K.next $ mainScene mjs pad
      | PDown `elem` as   = K.push $ mainScene mjs pad
      | Exit  `elem` as   = K.end
      --
      | otherwise         = K.continue
      where
        cnt = gCount g

    targetCount = 5 :: Int

pauseScene :: Metapad Action -> Scene Game IO Action
pauseScene pad = Scene pad update render transit allocGame
  where
    update _ _ = return

    render _ _ = do
      K.clearBy $ V4 50 50 0 255
      K.printTest (P (V2 10 100)) (V4 255 255 255 255) "PAUSE"

    transit _ as _
      | Enter `elem` as = K.end
      | otherwise       = K.continue

clearScene :: Maybe K.Joystick -> Int -> Metapad Action -> Scene Game IO Action
clearScene mjs score pad = Scene pad update render transit allocGame
  where
    update _ _ = return

    render _ _ = do
      K.clearBy $ V4 0 0 255 255
      K.printTest (P (V2 10 100)) (V4 255 255 255 255) "CLEAR!"
      K.printTest (P (V2 10 120)) (V4 255 255 255 255) $ T.pack ("Score: " ++ show score)
      K.printTest (P (V2 10 140)) (V4 255 255 255 255) "Enter - title"

    transit _ as _g
      | Enter `elem` as = K.next $ titleScene mjs pad
      | otherwise       = K.continue
