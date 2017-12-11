{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Scene where

-- import           Debug.Trace           (traceM)

import           Control.Lens
import           Control.Monad.Extra   (whenJust)
import           Control.Monad.State
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import qualified Data.Vector           as V
import           Linear.V4

import qualified SDL
import qualified SDL.Primitive         as Gfx

import qualified Kurokos               as K
import qualified Kurokos.Asset         as Asset
import qualified Kurokos.Asset.SDL     as Asset
import           Kurokos.UI            (UExp (..), ctxAttrib, visible)
import qualified Kurokos.UI            as UI
import qualified Kurokos.UI.Cursor     as UI

import           Import

import           Pad

data Dummy = Dummy

data MyData = MyData
  { gTexture :: SDL.Texture
  , gDeg     :: !Double
  , gCount   :: !Int
  , gActions :: [Action]
  }

allocGame :: ResourceT (KurokosT IO) MyData
allocGame = do
  (_, tex) <- K.allocTexture "_data/img.png"
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
        forM_ (s^.msLClicks) $ \p ->
          Gfx.fillCircle r p 5 (V4 255 255 0 255)
        forM_ (s^.msRClicks) $ \p ->
          Gfx.fillCircle r p 5 (V4 255 0 255 255)

    transit _ as _
      | Enter `elem` as = K.end
      | otherwise       = K.continue

data Title = Title
  { _tGui    :: UI.GUI
  , _tCursor :: UI.Cursor
  , _tCnt    :: Int
  }

makeLenses ''Title

titleScene :: Scene Title IO Action
titleScene =
  Scene defPad update render transit alloc
  where
    nameMain = "go-main"
    nameMouse = "go-mouse"

    alloc = do
      guiYaml <- liftIO $ B.readFile "_data/gui-title.yaml"
      assetList <- liftIO $ do
        assets1 <- Asset.decodeAssetList =<< B.readFile "_data/assets1.yaml"
        assets2 <- Asset.decodeAssetList =<< B.readFile "_data/assets2.yaml"
        return $ assets1 <> assets2
      astMng <- Asset.loadAssetManager assetList
      r <- K.getRenderer
      (_,sdlAssets) <- allocate (Asset.genSDLAssetManager r astMng) Asset.freeSDLAssetManager
      gui <- UI.newGui (UI.GuiEnv colset sdlAssets) $ do
        -- Label
        let size0 = V2 (Rpn "$width") (C 40)
            pos = V2 (C 0) (C 30)
        label <- UI.genSingle (Just "title") pos size0 =<< UI.newLabel "robotoj-b" "Kurokos デモ"
        -- Buttons
        let size = V2 (Rpn "0.4 $width *") (C 40)
            pos1 = V2 (Rpn "0.3 $width *") (Rpn "0.2 $height *")
            pos2 = V2 (Rpn "0.3 $width *") (Rpn "0.2 $height * 50 +")
        button1 <- UI.genSingle (Just nameMain) pos1 size =<< UI.newButton "robotoj-r" "Next: Main Scene"
        button2 <- UI.genSingle (Just nameMouse) pos2 size =<< UI.newButton "robotoj-r" "Push: Mouse Scene"
        -- Image
        let imgSize = V2 (C 48) (C 48)
            imgPos = V2 (C 10) (Rpn "$height 58 -")
        img <- UI.genSingle (Just "image") imgPos imgSize =<< UI.newImageView "sample-image"
        --
        let size' = V2 (Rpn "$width") (Rpn "$height 2 /")
        lbl' <- UI.genSingle (Just "label") (V2 (C 0) (C 0)) size' =<< UI.newLabel "robotoj-b" "---"
        btn' <- UI.genSingle (Just "button") (V2 (C 0) (Rpn "$height 2 /")) size' =<< UI.newButton "robotoj-m" "Button in Container"
        ctn1 <- UI.genContainer Nothing UI.Unordered (V2 (Rpn "$width 2 /") (Rpn "$height 2 /")) (V2 (C 200) (C 100))
        let Just ctn1' = UI.appendChild (mconcat [lbl', btn']) ctn1
        --
        btns <- mconcat <$> mapM (UI.genSingle Nothing (V2 (C 0) (C 0)) (V2 (Rpn "$width") (C 30)) <=< UI.newButton "robotoj-m" . T.pack . show) [1..(5::Int)]
        ctn2 <- UI.genContainer (Just "menu") UI.VerticalStack (V2 (Rpn "$width 140 -") (C 0)) (V2 (C 100) (C 300))
        let Just ctn2' = UI.appendChild btns ctn2
        --
        clickableArea <- UI.genSingle (Just "clickable") (V2 (C 0) (C 0)) (V2 (Rpn "$width") (Rpn "$height")) =<< UI.newTransparent
        fill <- UI.genSingle (Just "fill") (V2 (C 0) (C 0)) (V2 (Rpn "$width") (Rpn "$height")) =<< UI.newFill
        --
        UI.prependRoot $ mconcat [clickableArea, label, button1, button2, img, ctn1', fill, ctn2']

        -- Modify attribute
        modify' $ UI.update "clickable" (set (_1 . UI.ctxAttrib . UI.clickable) True)
        modify' $ UI.update "fill" (set (_1 . UI.ctxAttrib . UI.visible) False)

        -- From file
        UI.appendRoot =<< UI.newWidgetTreeFromData guiYaml

      liftIO . putStrLn . UI.pretty $ UI.getWidgetTree gui
      liftIO . putStrLn . UI.showTree $ UI.getWidgetTree gui
      return $ Title gui UI.makeCursor 0
      where
        colset = UI.ColorSet wcol wcmod

        wcol =
          UI.WC UI.WP
            { UI.wpBack = V4 255 255 255 255
            , UI.wpTint = V4 220 220 220 255
            , UI.wpFont = V4 54 20 171 255
            }

        wcmod = UI.WCM $ UI.WP toHover toHover id
          where
            toHover :: UI.Color -> UI.Color
            toHover = over _xyz (fmap (+ (-10)))

    update :: Update Title IO Action
    update _st _as title0 =
      readyG . testOnClick =<< updateT title0 =<< K.getEvents
      where
        updateT t0 es = do
          t1 <- t0 & tCursor %%~ UI.updateCursor es
          t1 & tGui %%~ UI.updateGui es (t1^.tCursor)
        readyG t = t & tGui %%~ UI.readyRender

        testOnClick t =
          flip execState t $ do
            -- click
            let modGui = modify' . over tGui
            whenJust (UI.clicked "fill" gui0) $ \_ -> do
              modGui $ UI.update "menu" $ set (_1.ctxAttrib.visible) False
              modGui $ UI.update "fill" $ set (_1.ctxAttrib.visible) False
            whenJust (UI.clicked "clickable" gui0) $ \(pos,btn) ->
              when (btn == SDL.ButtonRight) $ do
                modGui $ UI.update "menu" $ set (_1.ctxAttrib.visible) True
                modGui $ UI.setGlobalPosition "menu" pos
                modGui $ UI.update "fill" $ set (_1.ctxAttrib.visible) True
            -- update title
            whenJust (UI.clicked "button" gui0) $ \(_,btn) ->
              when (btn == SDL.ButtonLeft) $ do
                modify' $
                  over tCnt (+1)
                cnt <- gets $ view tCnt
                let title = T.pack $ show cnt
                modify' $
                  over tGui $ UI.update "label" (over _2 (UI.setTitle title))
          where
            gui0 = t^.tGui

    render :: Render Title IO
    render _ t = do
      K.clearBy $ V4 250 250 250 255
      --
      K.printTest (P (V2 10 200)) color "- Joysticks"
      vjs <- K.getJoysticks
      let showjs js = "#" <> T.pack (show (K.jsId js)) <> ": " <> K.jsDeviceName js
      V.imapM_ (\i js -> K.printTest (P $ V2 10 (220 + i * 20)) color (showjs js)) vjs
      --
      UI.render $ t^.tGui
      K.withRenderer $ \r -> do
        let P pos = UI._cursorPos $ t^.tCursor
        Gfx.smoothCircle r pos 5 (V4 0 0 0 255)
      return ()
      where
        color = V4 50 50 50 255

    transit _ as t
      | Exit `elem` as    = K.end
      | onClick nameMain  = K.next mainScene
      | onClick nameMouse = K.push mouseScene
      | otherwise         = K.continue
      where
        gui = t^.tGui
        onClick ident =
          case UI.clicked ident gui of
            Nothing      -> False
            Just (_,btn) -> btn == SDL.ButtonLeft

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
          -- c <- gets gCount
          -- let strength = fromIntegral c * 0.2
          --     len = fromIntegral c * 100
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
                dx :: Double
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
