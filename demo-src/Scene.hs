{-# LANGUAGE OverloadedStrings #-}
module Scene where

import           Control.Monad.State
import qualified Data.Text           as T

import qualified SDL
import qualified SDL.Font            as Font
import qualified SDL.Primitive       as Gfx

import qualified Kurokos             as K

import           Import

import           Pad

data Title = Title

data Game = Game
  { gTexture :: SDL.Texture
  , gDeg     :: !Double
  , gCount   :: !Int
  , gActions :: [Action]
  }

allocGame :: ResourceT (KurokosT IO) Game
allocGame = do
  env <- lift K.getEnv
  (_, tex) <- K.allocTexture "_data/img.png"
  (_, font) <- allocate (Font.load fontPath 50) Font.free

  return $ Game tex 0 0 []
  where
    fontPath = "_data/system.ttf"

--

titleScene :: Maybe K.Joystick -> Metapad Action -> Scene Title IO Action
titleScene mjs pad =
  Scene pad update render transit (return Title)
  where
    update :: Update Title IO Action
    update _ _as t = return t

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
      K.setAlphaMod (gTexture g0) alpha
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
    render sst (Game tex deg cnt as) = do
      K.clearBy $ V4 0 0 0 255

      K.withRenderer $ \r -> do
        let rect = SDL.Rectangle (SDL.P $ V2 50 200) (V2 50 50)
        SDL.copyEx r tex Nothing (Just rect) (realToFrac deg) Nothing (pure False)

      K.withRenderer $ \r -> do
        let p0 = V2 200 250
            p1 = p0 + (round <$> (V2 dx dy ^* 30))
              where
                dx = cos $ fromIntegral t / 5
                dy = sin $ fromIntegral t / 5
        Gfx.thickLine r p0 p1 4 (V4 0 255 0 255)

      K.printTest (P (V2 10 100)) color "Press Enter key to pause"
      K.printTest (P (V2 10 120)) color "Press F key!"
      let progress = replicate cnt '>' ++ replicate (targetCount - cnt) '-'
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
      K.printTest (P (V2 10 120)) (V4 255 255 255 255) $ "Score: " <> T.pack (show score)
      K.printTest (P (V2 10 140)) (V4 255 255 255 255) "Enter - title"

    transit _ as _g
      | Enter `elem` as = K.next $ titleScene mjs pad
      | otherwise       = K.continue
