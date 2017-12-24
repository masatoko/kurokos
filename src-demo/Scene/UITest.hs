{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Scene.UITest
  ( runUITestScene
  ) where

import           Control.Lens
import           Control.Monad.Extra          (whenJust)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8        as B
import           Data.Maybe                   (isJust)
import qualified Data.Text                    as T
import           Linear.V4

import qualified SDL

import qualified Kurokos                      as K
import qualified Kurokos.Asset                as Asset
import qualified Kurokos.Asset.Raw            as Asset
import qualified Kurokos.Graphics             as G
import           Kurokos.UI                   (UExp (..), clickable, ctxAttrib,
                                               visible)
import qualified Kurokos.UI                   as UI

import           Action                       (Action (..), eventsToActions)
import           Game
import           Import

import           Graphics.Rendering.OpenGL    (($=))
import qualified Graphics.Rendering.OpenGL    as GL

import qualified Scene.Game

data Dummy = Dummy [Action]

data MyData = MyData
  { gTexture   :: G.Texture
  , gDeg       :: !Float
  , gCount     :: !Int
  , gMyActions :: [Action]
  }

-- data UserVal = UserVal
--   { _uvMVar :: MVar.MVar Int
--   , _uvFont :: Font.Font
--   }
--
-- makeLenses ''UserVal
--
-- instance UI.Renderable UserVal where
--   renderW r _sz wcol (UserVal mvar font) = do
--     cnt <- MVar.readMVar mvar
--     let text = T.pack $ show cnt
--     (w,h) <- Font.size font text
--     let size = fromIntegral <$> V2 w h
--     tex <- E.bracket
--       (Font.blended font (UI._wcTitle wcol) text)
--       SDL.freeSurface
--       (SDL.createTextureFromSurface r)
--     SDL.copy r tex Nothing (Just $ SDL.Rectangle (pure 0) size)
--
--   needsRender (UserVal mvar _) = do
--     cnt <- MVar.readMVar mvar
--     return $ cnt `mod` 10 == 0

data UITest = UITest
  { _tGui    :: UI.GUI
  , _tCursor :: UI.Cursor
  -- , _tUserVal :: UserVal
  , _tEvents :: [(UI.GuiAction, UI.GuiEvent)]
  , _tCnt    :: Int
  }

makeLenses ''UITest

runUITestScene :: KurokosT (GameT IO) ()
runUITestScene =
  runResourceT $ do
    t <- alloc
    lift $ runScene scene t
  where
    scene = Scene update render transit

    nameMain = "go-main"

    alloc = do
      guiYaml <- liftIO $ B.readFile "_data/gui-title.yaml"
      assetList <- liftIO $ do
        assets1 <- Asset.decodeAssetList =<< B.readFile "_data/assets1.yaml"
        assets2 <- Asset.decodeAssetList =<< B.readFile "_data/assets2.yaml"
        return $ assets1 <> assets2
      astMng <- Asset.loadAssetManager assetList
      r <- K.getRenderer
      (_,sdlAssets) <- allocate (Asset.newAssetManager r astMng) Asset.freeAssetManager
      --
      -- userVal <- liftIO $ UserVal <$> MVar.newMVar 0 <*> Asset.getFont "font-r" 16 sdlAssets
      --
      colorScheme <- liftIO $ UI.readColorScheme "_data/gui-color-scheme.yaml"
      gui <- UI.newGui (UI.GuiEnv sdlAssets colorScheme) $ do
        -- Label
        let size0 = V2 (Rpn "$width") (C 40)
            pos = V2 (C 0) (C 30)
        label <- UI.mkSingle (Just "title") Nothing pos size0 =<< UI.newLabel "font-m" 18 "Kurokos DEMO"
        -- Buttons
        let size = V2 (Rpn "0.4 $width *") (C 40)
            pos1 = V2 (Rpn "0.3 $width *") (Rpn "0.2 $height *")
            pos2 = V2 (Rpn "0.3 $width *") (Rpn "0.2 $height * 50 +")
        button1 <- UI.mkSingle (Just nameMain) Nothing pos1 size =<< UI.newButton "font-m" 16 "Next: Main Scene"
        -- Image
        let imgSize = V2 (C 48) (C 48)
            imgPos = V2 (C 10) (Rpn "$height 58 -")
        img <- UI.mkSingle (Just "image") Nothing imgPos imgSize =<< UI.newImageView "sample-image"
        -- UserWidget
        -- userWidget <- UI.mkSingle (Just "user_widget") Nothing (pure (C 0)) (pure (C 100)) $ UI.UserWidget userVal
        --
        let size' = V2 (Rpn "$width") (Rpn "$height 2 /")
        lbl' <- UI.mkSingle (Just "label") Nothing (V2 (C 0) (C 0)) size' =<< UI.newLabel "font-m" 16 "---"
        btn' <- UI.mkSingle (Just "button") Nothing (V2 (C 0) (Rpn "$height 2 /")) size' =<< UI.newButton "font-m" 16 "Button in Container"
        ctn1 <- UI.mkContainer Nothing UI.Unordered Nothing (V2 (Rpn "$width 2 /") (Rpn "$height 2 /")) (V2 (C 200) (C 100))
        let Just ctn1' = UI.appendChild (mconcat [lbl', btn']) ctn1
        --
        btns <- mconcat <$> mapM (UI.mkSingle Nothing Nothing (V2 (C 0) (C 0)) (V2 (Rpn "$width") (C 30)) <=< UI.newButton "font-m" 16 . T.pack . show) [1..(5::Int)]
        ctn2 <- UI.mkContainer (Just "menu") UI.VerticalStack Nothing (V2 (Rpn "$width 140 -") (C 0)) (V2 (C 100) (C 300))
        let Just ctn2' = UI.appendChild btns ctn2
        --
        clickableArea <- UI.mkSingle (Just "clickable") Nothing (V2 (C 0) (C 0)) (V2 (Rpn "$width") (Rpn "$height")) =<< UI.newTransparent
        fill <- UI.mkSingle (Just "fill") Nothing (V2 (C 0) (C 0)) (V2 (Rpn "$width") (Rpn "$height")) =<< UI.newFill
        --
        UI.prependRoot $ mconcat [clickableArea, label, button1, img, ctn1', fill, ctn2']
        -- UI.prependRoot $ mconcat [clickableArea, label, button1, button2, img, userWidget, ctn1', fill, ctn2']
        -- From file
        UI.appendRoot =<< UI.parseWidgetTree guiYaml

      let gui' = flip execState gui $ do
                   -- Modify attribute
                   modify' $ UI.update "clickable" (set (_1.ctxAttrib.clickable) True)
                   modify' $ UI.update "fill" (set (_1.ctxAttrib.visible) False)

      liftIO . putStrLn . UI.pretty $ UI.getWidgetTree gui'
      liftIO . putStrLn . UI.showTree $ UI.getWidgetTree gui'
      cursor <- UI.newCursor
      return $ UITest gui' cursor [] 0
      -- return $ UITest gui' cursor userVal [] 0

    update :: Update (GameT IO) UITest
    update title0 = do
      es <- K.getEvents
      -- liftIO $ MVar.modifyMVar_ (title0^.tUserVal.uvMVar) (return . (+1)) -- Update UserVal
      readyG . work . updateEs es =<< updateT es title0
      where
        updateT es t0 = do
          t1 <- t0 & tCursor %%~ UI.updateCursor es
          t1 & tGui %%~ UI.updateGui es (t1^.tCursor)
        updateEs esSDL t = t & tEvents .~ es
          where
            es = UI.handleGui UI.defaultGuiHandler esSDL (t^.tCursor) (t^.tGui)
        readyG t = t & tGui %%~ UI.readyRender

        work t =
          flip execState t $ do
            whenJust (UI.clickedOn UI.GuiActLeft "fill" es) $ \_ -> do
              modGui $ UI.update "menu" $ set (_1.ctxAttrib.visible) False
              modGui $ UI.update "fill" $ set (_1.ctxAttrib.visible) False

            whenJust (UI.clickedOn UI.GuiActRight "clickable" es) $ \pos -> do -- TODO: Right click
              modGui $ UI.update "menu" $ set (_1.ctxAttrib.visible) True
              modGui $ UI.setGlobalPosition "menu" pos
              modGui $ UI.update "fill" $ set (_1.ctxAttrib.visible) True

            -- -- ** Update title
            -- whenJust (UI.clickedOn UI.GuiActLeft "button" es) $ \_pos -> do
            --   modify' $
            --     over tCnt (+1)
            --   cnt <- gets $ view tCnt
            --   let title = T.pack $ show cnt
            --   modGui $ UI.update "label" (over _2 (UI.setTitle title))
          where
            es = t^.tEvents
            modGui = modify' . over tGui

    render :: Render (GameT IO) UITest
    render t = do
      clearBy $ V4 0.97 0.97 0.97 1
      --
      -- K.printTest (V2 10 200) color "- Joysticks"
      -- vjs <- K.getJoysticks
      -- let showjs js = "#" <> T.pack (show (K.jsId js)) <> ": " <> K.jsDeviceName js
      -- V.imapM_ (\i js -> K.printTest (V2 10 (220 + i * 20)) color (showjs js)) vjs
      --
      UI.render $ t^.tGui
      -- K.withRenderer $ \r -> do
      --   let P pos = t^.tCursor.cursorPos
      --   Prim.smoothCircle r pos 5 (V4 0 0 0 255) -- Cursor
      return ()
      -- where
      --   color = V3 50 50 50

    transit t = do
      when (isClicked nameMain) Scene.Game.runMainScene
      K.continue t
      where
        isClicked name = isJust $ UI.clickedOn UI.GuiActLeft name $ t^.tEvents

clearBy :: MonadIO m => V4 Float -> m ()
clearBy (V4 r g b a) = liftIO $ do
  GL.clearColor $= GL.Color4 r g b a
  GL.clear [GL.ColorBuffer]
