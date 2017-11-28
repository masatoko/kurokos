{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Scene where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.Vector         as V

import qualified SDL
import qualified SDL.Font            as Font
import qualified SDL.Primitive       as Gfx

import qualified Kurokos             as K
import qualified Kurokos.GUI         as GUI

import Kurokos.GUI.Core

import           Import

import           Pad

fontPath :: FilePath
fontPath = "_data/system.ttf"

data Dummy = Dummy

data MyData = MyData
  { gTexture :: SDL.Texture
  , gDeg     :: !Double
  , gCount   :: !Int
  , gActions :: [Action]
  }

allocGame :: ResourceT (KurokosT IO) MyData
allocGame = do
  env <- lift K.getEnv
  (_, tex) <- K.allocTexture "_data/img.png"
  (_, font) <- allocate (Font.load fontPath 50) Font.free

  return $ MyData tex 0 0 []

--

data MouseScene = MouseScene
  { _msLClicks :: [V2 CInt]
  , _msRClicks :: [V2 CInt]
  } deriving Show

makeLenses ''MouseScene

mouseScene :: Scene MouseScene IO Action
mouseScene = Scene defPad update render transit (pure (MouseScene [] []))
  where
    update _ _ s = do
      es <- K.getEvents
      execStateT (mapM_ go es) s
      where
        go (SDL.MouseButtonEvent dt) = do
          liftIO $ print dt
          when (SDL.mouseButtonEventMotion dt == SDL.Pressed) $ do
            let (P pos) = SDL.mouseButtonEventPos dt
                pos' = fromIntegral <$> pos
            case SDL.mouseButtonEventButton dt of
              SDL.ButtonLeft  -> msLClicks %= (pos':)
              SDL.ButtonRight -> msRClicks %= (pos':)
              _               -> return ()
        go _ = return ()

    render _ s = do
      P pos <- SDL.getAbsoluteMouseLocation
      let pos' = fromIntegral <$> pos
      K.withRenderer $ \r ->
        Gfx.smoothCircle r pos' 5 (V4 255 255 255 255)
      --
      K.withRenderer $ \r -> do
        forM_ (s^.msLClicks) $ \pos ->
          Gfx.fillCircle r pos 5 (V4 255 255 0 255)
        forM_ (s^.msRClicks) $ \pos ->
          Gfx.fillCircle r pos 5 (V4 255 0 255 255)

    transit _ as _
      | Enter `elem` as = K.end
      | otherwise       = K.continue

newtype Title = Title GUI.GUI

titleScene :: Scene Title IO Action
titleScene =
  Scene defPad update render transit alloc
  where
    alloc = do
      (_, font) <- allocate (K.loadFont (K.FontFile fontPath) 20) K.freeFont
      let env = GUI.GuiEnv font
      gui <- GUI.newGui env $ do
        let size = V2 (GUI.UERPN "100") (GUI.UERPN "100")
            pos1 = V2 (GUI.UERPN "0.5 $w *") (GUI.UERPN "0.2 $h *")
            pos2 = V2 (GUI.UERPN "10") (GUI.UERPN "0.2 $h * 50 +")
        label1 <- GUI.genSingle pos1 size =<< GUI.newLabel "label1"
        label2 <- GUI.genSingle pos2 size =<< GUI.newLabel "label2"
        prependRootWs [label1, label2]
      liftIO . print $ getWidgetTrees gui
      return $ Title gui
      where

    update :: Update Title IO Action
    update _ _as = return

    render :: Render Title IO
    render _ (Title gui) = do
      K.clearBy $ V4 100 100 100 255
      --
      K.printTest (P (V2 10 100)) white "Enter - start"
      K.printTest (P (V2 10 120)) white "Escape - exit"
      K.printTest (P (V2 10 160)) white "日本語テスト"
      --
      K.printTest (P (V2 10 200)) white "- Joysticks"
      vjs <- K.getJoysticks
      let showjs js = "#" <> T.pack (show (K.jsId js)) <> ": " <> K.jsDeviceName js
      V.imapM_ (\i js -> K.printTest (P $ V2 10 (220 + i * 20)) white (showjs js)) vjs
      --
      gui' <- renderGUI gui
      return ()
      where
        white = V4 255 255 255 255

    transit _ as _
      | Enter `elem` as = K.next mainScene
      | Go    `elem` as = K.push mouseScene
      | Exit  `elem` as = K.end
      | otherwise       = K.continue

mainScene :: Scene MyData IO Action
mainScene = Scene defPad update render transit allocGame
  where
    update :: Update MyData IO Action
    update stt as g0 = do
      -- when (frameCount stt `mod` 60 == 0) $ K.averageTime >>= liftIO . print
      let alpha = fromIntegral $ frameCount stt
      K.setAlphaMod (gTexture g0) alpha
      execStateT go g0
      where
        go :: StateT MyData (KurokosT IO) ()
        go = do
          mapM_ count as
          setDeg
          unless (null as) $ modify $ \g -> g {gActions = as}

        count :: Action -> StateT MyData (KurokosT IO) ()
        count Go = do
          modify (\a -> let c = gCount a in a {gCount = c + 1})
          c <- gets gCount
          let strength = fromIntegral c * 0.2
              len = fromIntegral c * 100
          -- mapM_ (\joy -> K.rumble joy strength len) mjs
          return ()
        count _  = return ()

        setDeg = modify (\g -> g {gDeg = fromIntegral (frameCount stt `mod` 360)})

    render :: Render MyData IO
    render sst (MyData tex deg cnt as) = do
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
      | cnt > targetCount = K.next $ clearScene cnt
      | Enter `elem` as   = K.push pauseScene
      --
      | PUp   `elem` as   = K.next mainScene
      | PDown `elem` as   = K.push mainScene
      | Exit  `elem` as   = K.end
      --
      | otherwise         = K.continue
      where
        cnt = gCount g

    targetCount = 5 :: Int

pauseScene :: Scene Dummy IO Action
pauseScene = Scene defPad update render transit (pure Dummy)
  where
    update _ _ = return

    render _ _ = do
      K.clearBy $ V4 50 50 0 255
      K.printTest (P (V2 10 100)) (V4 255 255 255 255) "PAUSE"

    transit _ as _
      | Enter `elem` as = K.end
      | otherwise       = K.continue

clearScene :: Int -> Scene Dummy IO Action
clearScene score = Scene defPad update render transit (pure Dummy)
  where
    update _ _ = return

    render _ _ = do
      K.clearBy $ V4 0 0 255 255
      K.printTest (P (V2 10 100)) (V4 255 255 255 255) "CLEAR!"
      K.printTest (P (V2 10 120)) (V4 255 255 255 255) $ "Score: " <> T.pack (show score)
      K.printTest (P (V2 10 140)) (V4 255 255 255 255) "Enter - title"

    transit _ as _g
      | Enter `elem` as = K.next titleScene
      | otherwise       = K.continue
