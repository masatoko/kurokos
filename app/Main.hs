{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           System.Environment (getArgs)
import           Control.Monad.State
import qualified Data.ByteString     as B
import           Data.Int            (Int16, Int32)
import qualified Data.Text           as T
import           Linear.V2
import           Linear.V4
import           Linear.Affine

import qualified SDL

import           Protonic            (Joystick, Metapad, ProtoT, Render,
                                      Scene (..), SceneState (..), Update,
                                      addAction, newPad, runProtoT, runScene,
                                      withProtonic)
import qualified Protonic            as P

data Title = Title

data Game = Game
  { gSprite  :: P.Sprite
  , gImgSprite :: P.Sprite
  , gDeg     :: !Double
  , gCount   :: !Int
  , gActions :: [Action]
  }

initGame :: ProtoT Game
initGame = do
  pconf <- P.getProtoConfig
  liftIO $ P.runProtoConfT pconf $ do
    font <- P.newFont fontPath 50
    char <- P.newSprite font (V4 255 255 255 255) "@"
    img <- P.loadSprite "_data/img.png" (pure 48)
    P.freeFont font
    liftIO . putStrLn $ "init Game"
    return $ Game char img 0 0 []
  where
    fontPath = "_data/system.ttf"

freeGame :: MonadIO m => Game -> m ()
freeGame g = liftIO $ do
  P.freeSprite . gSprite $ g
  putStrLn "free Game"

main :: IO ()
main = do
  as <- getArgs
  fontBytes <- B.readFile "_data/system.ttf"
  let opt = (`elem` as)
      conf = mkConf (opt "button") (opt "axis") (opt "hat")
      conf' = conf {P.confFont = Left fontBytes}
  withProtonic conf' $ \proto -> do
    mjs <- P.newJoystickAt 0
    let gamepad = mkGamepad mjs
    _ <- runProtoT proto $
      runScene $ titleScene mjs gamepad
    maybe (return ()) P.freeJoystick mjs
    return ()
  where
    mkConf pBtn pAxis pHat =
      P.defaultConfig
        { P.confWinSize = V2 640 480
        , P.confWinTitle = "protpnic-app"
        -- , P.confWindowMode = SDL.Fullscreen
        , P.confWindowMode = SDL.Windowed
        , P.confDebugPrintSystem = True
        , P.confDebugJoystick = P.DebugJoystick pBtn pAxis pHat
        }

    -- monitor mjs =
    --   case mjs of
    --     Nothing -> return ()
    --     Just js -> forever $ do
    --       clearScreen
    --       SDL.pumpEvents
    --       P.monitorJoystick js
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
  modify . addAction $ P.released SDL.ScancodeF Go
  modify . addAction $ P.pressed SDL.ScancodeReturn Enter
  modify . addAction $ P.pressed SDL.ScancodeEscape Exit
  -- Joystick
  case mjs of
    Just js -> do
      -- Buttons
      modify . addAction $ P.joyPressed js 4 Enter
      mapM_ (modify . addAction . uncurry (P.joyPressed js))
        [ (10, Go), (11, Go), (12, Go), (13, Go) ]
      -- Axes
      modify . addAction $ P.joyAxis2 js 0 1 AxisLeft
      -- Hat
      modify . addAction $ P.joyHat P.HDUp P.Pressed PUp
      modify . addAction $ P.joyHat P.HDUp P.Released RUp
      modify . addAction $ P.joyHat P.HDUp P.Holded HUp
      modify . addAction $ P.joyHat P.HDDown P.Pressed PDown
    Nothing -> return ()
  -- Mouse
  modify . addAction $ P.mouseButtonAct P.ButtonLeft P.Pressed Go
  modify . addAction $ P.mousePosAct MousePos
  modify . addAction $ P.mouseMotionAct MouseMotion
  modify . addAction $ P.mouseWheelAct MouseWheel
  -- Touch
  modify . addAction $ P.touchMotionAct TouchMotion

titleScene :: Maybe P.Joystick -> Metapad Action -> Scene Title Action
titleScene mjs pad =
  Scene pad update render transit (return Title) (\_ -> return ())
  where
    update :: Update Title Action
    update _ as t = return t

    render :: Render Title
    render _ _ = do
      P.printTest (P (V2 10 100)) (V4 0 255 255 255) "Enter - start"
      P.printTest (P (V2 10 120)) (V4 0 255 255 255) "Escape - exit"
      P.printTest (P (V2 10 160)) (V4 0 255 255 255) "日本語テスト"

    transit _ as _
      | Enter `elem` as = P.next $ mainScene mjs pad
      | Exit  `elem` as = P.end
      | otherwise       = P.continue

mainScene :: Maybe P.Joystick -> Metapad Action -> Scene Game Action
mainScene mjs pad = Scene pad update render transit initGame freeGame
  where
    update :: Update Game Action
    update stt as g0 = do
      -- when (frameCount stt `mod` 60 == 0) $ P.averageTime >>= liftIO . print
      let alpha = fromIntegral $ frameCount stt
      P.setAlphaMod (gImgSprite g0) alpha
      execStateT go g0
      where
        go :: StateT Game ProtoT ()
        go = do
          mapM_ count as
          setDeg
          unless (null as) $ modify $ \g -> g {gActions = as}

        count :: Action -> StateT Game ProtoT ()
        count Go = do
          modify (\a -> let c = gCount a in a {gCount = c + 1})
          c <- gets gCount
          let strength = fromIntegral c * 0.2
              len = fromIntegral c * 100
          mapM_ (\joy -> P.rumble joy strength len) mjs
        count _  = return ()

        setDeg = modify (\g -> g {gDeg = fromIntegral (frameCount stt `mod` 360)})

    render :: Render Game
    render _ (Game spr img d i as) = do
      P.clearBy $ V4 0 0 0 255
      P.renderS spr (P (V2 150 200)) Nothing (Just d)
      P.renderS img (P (V2 10 200)) Nothing Nothing
      P.setColor $ V4 0 255 0 255
      P.drawLine (P (V2 200 200)) (P (V2 270 230))
      --
      P.printTest (P (V2 10 100)) color "Press Enter key to pause"
      P.printTest (P (V2 10 120)) color "Press F key!"
      let progress = replicate i '>' ++ replicate (targetCount - i) '-'
      P.printTest (P (V2 10 140)) color $ T.pack progress
      P.printTest (P (V2 10 160)) color $ T.pack $ show as
      where
        color = V4 255 255 255 255

    transit _ as g
      | cnt > targetCount = P.next $ clearScene mjs cnt pad
      | Enter `elem` as   = P.push $ pauseScene pad
      --
      | PUp   `elem` as   = P.next $ mainScene mjs pad
      | PDown `elem` as   = P.push $ mainScene mjs pad
      | Exit  `elem` as   = P.end
      --
      | otherwise         = P.continue
      where
        cnt = gCount g

    targetCount = 5 :: Int

pauseScene :: Metapad Action -> Scene Game Action
pauseScene pad = Scene pad update render transit initGame freeGame
  where
    update _ _ = return

    render _ _ = do
      P.clearBy $ V4 50 50 0 255
      P.printTest (P (V2 10 100)) (V4 255 255 255 255) "PAUSE"

    transit _ as _
      | Enter `elem` as = P.end
      | otherwise       = P.continue

clearScene :: Maybe P.Joystick -> Int -> Metapad Action -> Scene Game Action
clearScene mjs score pad = Scene pad update render transit initGame freeGame
  where
    update _ _ = return

    render _ _ = do
      P.clearBy $ V4 0 0 255 255
      P.printTest (P (V2 10 100)) (V4 255 255 255 255) "CLEAR!"
      P.printTest (P (V2 10 120)) (V4 255 255 255 255) $ T.pack ("Score: " ++ show score)
      P.printTest (P (V2 10 140)) (V4 255 255 255 255) "Enter - title"

    transit _ as _g
      | Enter `elem` as = P.next $ titleScene mjs pad
      | otherwise       = P.continue
