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
import           Data.Maybe            (isJust)
import qualified Data.Text             as T
import qualified Data.Vector           as V
import           Linear.V4

import qualified SDL
import qualified SDL.Primitive         as Prim

import qualified Kurokos               as K
import qualified Kurokos.Asset         as Asset
import qualified Kurokos.Asset.SDL     as Asset
import           Kurokos.UI            (UExp (..), ctxAttrib, visible)
import qualified Kurokos.UI            as UI

import           Action                (Action (..), eventsToActions)
import           Import

data Dummy = Dummy [Action]

data MyData = MyData
  { gTexture   :: SDL.Texture
  , gDeg       :: !Double
  , gCount     :: !Int
  , gMyActions :: [Action]
  }

allocGame :: ResourceT (KurokosT IO) MyData
allocGame = do
  (_, tex) <- K.allocTexture "_data/img.png"
  return $ MyData tex 0 0 []

--

data MouseScene = MouseScene
  { _msLClicks :: [V2 CInt]
  , _msRClicks :: [V2 CInt]
  , _msActions :: [Action]
  } deriving Show

makeLenses ''MouseScene

mouseScene :: Scene IO MouseScene
mouseScene = Scene update render transit (pure (MouseScene [] [] []))
  where
    update s = do
      es <- K.getEvents
      let as = eventsToActions es
      execStateT (mapM_ go es) $ s&msActions .~ as
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

    render s = do
      P pos <- SDL.getAbsoluteMouseLocation
      let pos' = fromIntegral <$> pos
      K.withRenderer $ \r ->
        Prim.smoothCircle r pos' 5 (V4 255 255 255 255)
      --
      K.withRenderer $ \r -> do
        forM_ (s^.msLClicks) $ \p ->
          Prim.fillCircle r p 5 (V4 255 255 0 255)
        forM_ (s^.msRClicks) $ \p ->
          Prim.fillCircle r p 5 (V4 255 0 255 255)

    transit s
      | Select `elem` as = K.end
      | Cancel `elem` as = K.end
      | otherwise        = K.continue
      where
        as = s^.msActions

data Title = Title
  { _tGui    :: UI.GUI
  , _tCursor :: UI.Cursor
  , _tEvents :: [(UI.GuiAction, UI.GuiEvent)]
  , _tCnt    :: Int
  }

makeLenses ''Title

titleScene :: Scene IO Title
titleScene =
  Scene update render transit alloc
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
      (_,sdlAssets) <- allocate (Asset.newSDLAssetManager r astMng) Asset.freeSDLAssetManager
      gui <- UI.newGui (UI.GuiEnv colset sdlAssets) $ do
        -- Label
        let size0 = V2 (Rpn "$width") (C 40)
            pos = V2 (C 0) (C 30)
        label <- UI.genSingle (Just "title") pos size0 =<< UI.newLabel "font-m" "Kurokos DEMO"
        -- Buttons
        let size = V2 (Rpn "0.4 $width *") (C 40)
            pos1 = V2 (Rpn "0.3 $width *") (Rpn "0.2 $height *")
            pos2 = V2 (Rpn "0.3 $width *") (Rpn "0.2 $height * 50 +")
        button1 <- UI.genSingle (Just nameMain) pos1 size =<< UI.newButton "font-m" "Next: Main Scene"
        button2 <- UI.genSingle (Just nameMouse) pos2 size =<< UI.newButton "font-m" "Push: Mouse Scene"
        -- Image
        let imgSize = V2 (C 48) (C 48)
            imgPos = V2 (C 10) (Rpn "$height 58 -")
        img <- UI.genSingle (Just "image") imgPos imgSize =<< UI.newImageView "sample-image"
        --
        let size' = V2 (Rpn "$width") (Rpn "$height 2 /")
        lbl' <- UI.genSingle (Just "label") (V2 (C 0) (C 0)) size' =<< UI.newLabel "font-m" "---"
        btn' <- UI.genSingle (Just "button") (V2 (C 0) (Rpn "$height 2 /")) size' =<< UI.newButton "font-m" "Button in Container"
        ctn1 <- UI.genContainer Nothing UI.Unordered (V2 (Rpn "$width 2 /") (Rpn "$height 2 /")) (V2 (C 200) (C 100))
        let Just ctn1' = UI.appendChild (mconcat [lbl', btn']) ctn1
        --
        btns <- mconcat <$> mapM (UI.genSingle Nothing (V2 (C 0) (C 0)) (V2 (Rpn "$width") (C 30)) <=< UI.newButton "font-m" . T.pack . show) [1..(5::Int)]
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
      cursor <- UI.newCursor
      return $ Title gui cursor [] 0
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

    update :: Update IO Title
    update title0 = do
      es <- K.getEvents
      readyG . testOnClick . updateEs es =<< updateT es title0
      where
        updateT es t0 = do
          t1 <- t0 & tCursor %%~ UI.updateCursor es
          t1 & tGui %%~ UI.updateGui es (t1^.tCursor)
        updateEs esSDL t = t & tEvents .~ es
          where
            es = UI.handleGui esSDL (t^.tCursor) (t^.tGui) UI.defaultGuiHandler
        readyG t = t & tGui %%~ UI.readyRender

        testOnClick t =
          flip execState t $ do
            whenJust (UI.clickedOn UI.GuiActLeft "fill" es) $ \_ -> do
              modGui $ UI.update "menu" $ set (_1.ctxAttrib.visible) False
              modGui $ UI.update "fill" $ set (_1.ctxAttrib.visible) False

            whenJust (UI.clickedOn UI.GuiActRight "clickable" es) $ \pos -> do -- TODO: Right click
              modGui $ UI.update "menu" $ set (_1.ctxAttrib.visible) True
              modGui $ UI.setGlobalPosition "menu" pos
              modGui $ UI.update "fill" $ set (_1.ctxAttrib.visible) True

            -- ** Update title
            whenJust (UI.clickedOn UI.GuiActLeft "button" es) $ \_pos -> do
              modify' $
                over tCnt (+1)
              cnt <- gets $ view tCnt
              let title = T.pack $ show cnt
              modify' $
                over tGui $ UI.update "label" (over _2 (UI.setTitle title))
          where
            es = t^.tEvents
            modGui = modify' . over tGui

    render :: Render IO Title
    render t = do
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
        Prim.smoothCircle r pos 5 (V4 0 0 0 255)
      return ()
      where
        color = V4 50 50 50 255

    transit t
      | isClicked nameMain  = K.next mainScene
      | isClicked nameMouse = K.push mouseScene
      | otherwise           = K.continue
      where
        isClicked ident = isJust $ UI.clickedOn UI.GuiActLeft ident $ t^.tEvents

mainScene :: Scene IO MyData
mainScene = Scene update render transit allocGame
  where
    update :: Update IO MyData
    update g0 = do
      as <- eventsToActions <$> K.getEvents
      frame <- K.getFrame
      work frame $ g0 {gMyActions = as}
      where
        work t g = do
          K.setAlphaMod (gTexture g0) alpha
          execStateT go g0
          where
            alpha = fromIntegral t
            go :: StateT MyData (KurokosT IO) ()
            go = do
              mapM_ count $ gMyActions g
              modify $ \g' -> g' { gDeg = fromIntegral (t `mod` 360) }

        count :: Action -> StateT MyData (KurokosT IO) ()
        count Select = modify (\a -> let c = gCount a in a {gCount = c + 1})
        count _      = return ()


    render :: Render IO MyData
    render (MyData tex deg cnt as) = do
      t <- K.getFrame
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
        Prim.thickLine r p0 p1 4 (V4 0 255 0 255)

      K.printTest (P (V2 10 100)) color "Press Enter key to pause"
      K.printTest (P (V2 10 120)) color "Press F key!"
      let progress = replicate cnt '>' ++ replicate (targetCount - cnt) '-'
      K.printTest (P (V2 10 140)) color $ T.pack progress
      K.printTest (P (V2 10 160)) color $ T.pack $ show as
      where
        color = V4 255 255 255 255

    transit g
      | cnt > targetCount = K.next $ clearScene cnt
      | Select `elem` as  = K.push pauseScene
      | Cancel `elem` as  = K.end
      | otherwise         = K.continue
      where
        cnt = gCount g
        as = gMyActions g

    targetCount = 5 :: Int

pauseScene :: Scene IO Dummy
pauseScene = Scene update render transit (pure $ Dummy [])
  where
    update _ =
      Dummy . eventsToActions <$> K.getEvents

    render _ = do
      K.clearBy $ V4 50 50 0 255
      K.printTest (P (V2 10 100)) (V4 255 255 255 255) "PAUSE"

    transit (Dummy as)
      | Select `elem` as = K.end
      | otherwise        = K.continue

clearScene :: Int -> Scene IO Dummy
clearScene score = Scene update render transit (pure $ Dummy [])
  where
    update _ =
      Dummy . eventsToActions <$> K.getEvents

    render _ = do
      K.clearBy $ V4 0 0 255 255
      K.printTest (P (V2 10 100)) (V4 255 255 255 255) "CLEAR!"
      K.printTest (P (V2 10 120)) (V4 255 255 255 255) $ "Score: " <> T.pack (show score)
      K.printTest (P (V2 10 140)) (V4 255 255 255 255) "Enter - title"

    transit (Dummy as)
      | Select `elem` as = K.next titleScene
      | otherwise        = K.continue