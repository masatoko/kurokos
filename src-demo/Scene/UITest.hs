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

-- import qualified SDL

import           Data.Default.Class           (def)
import qualified Kurokos                      as K
import qualified Kurokos.Asset                as Asset
import qualified Kurokos.Asset.Raw            as Asset
-- import qualified Kurokos.Graphics             as G
import           Kurokos.UI                   (UExp (..), clickable, ctxAttrib,
                                               visible)
import qualified Kurokos.UI                   as UI

-- import           Action                       (Action (..), eventsToActions)
import           Game
import           Import

import           Graphics.Rendering.OpenGL    (($=))
import qualified Graphics.Rendering.OpenGL    as GL

import           Scene                        (runTitleScene)

-- data Dummy = Dummy [Action]

-- data MyData = MyData
--   { gTexture   :: G.Texture
--   , gDeg       :: !Float
--   , gCount     :: !Int
--   , gMyActions :: [Action]
--   }

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
  { _tGui     :: UI.GUI
  , _tCursor  :: UI.Cursor
  -- , _tUserVal :: UserVal
  -- , _tCnt     :: Int
  }

makeLenses ''UITest

runUITestScene :: KurokosT (GameT IO) ()
runUITestScene =
  runResourceT $ do
    (t, sdlAssets) <- alloc
    lift $ runScene scene t
    Asset.freeAssetManager sdlAssets
  where
    scene = Scene update render transit

    nameMain = "go-main"

    alloc = do
      guiYaml <- liftIO $ B.readFile "_data/gui-uitest.yaml"
      assetList <- liftIO $ do
        assets1 <- Asset.decodeAssetList =<< B.readFile "_data/assets1.yaml"
        assets2 <- Asset.decodeAssetList =<< B.readFile "_data/assets2.yaml"
        return $ assets1 <> assets2
      astMng <- Asset.loadAssetManager assetList
      r <- K.getRenderer
      sdlAssets <- liftIO $ Asset.newAssetManager r astMng
      --
      -- userVal <- liftIO $ UserVal <$> MVar.newMVar 0 <*> Asset.getFont "font-r" 16 sdlAssets
      --
      stylemap <- liftIO $ UI.readStyleMap "_data/gui-stylemap.yaml"
      liftIO . print $ stylemap
      (_,gui) <- UI.newGui (UI.GuiEnv sdlAssets stylemap) $ do
        -- * Label
        let confL = UI.WidgetConfig (Just "title") Nothing Nothing (C 0) (C 30) (Rpn "$width") (C 40)
        label <- UI.mkSingle confL =<< UI.newLabel "font-m" 18 "Kurokos DEMO"
        -- * Buttons
        let confB = UI.WidgetConfig (Just nameMain) Nothing Nothing (Rpn "0.3 $width *") (Rpn "0.2 $height *") (Rpn "0.4 $width *") (C 40)
        button1 <- UI.mkSingle confB =<< UI.newButton "font-m" 16 "Next: Main Scene"
        -- * Image
        let confImg = UI.WidgetConfig (Just "image") Nothing Nothing (C 10) (Rpn "$height 58 -") (C 48) (C 48)
        img <- UI.mkSingle confImg =<< UI.newImageView "sample-image"
        -- * TextField
        let confTxtFld = UI.WidgetConfig Nothing Nothing Nothing (C 100) (C 200) (Rpn "$min-width") (C 20)
        texfld <- UI.mkSingle confTxtFld =<< UI.newTextField "font-m" 16 "initial text"
        -- * Picker
        let confPkr = UI.WidgetConfig Nothing Nothing Nothing (C 100) (C 250) (Rpn "$min-width") (C 20)
        picker <- UI.mkSingle confPkr =<< UI.newPicker "font-m" 16 [("1", "Elem1"), ("2", "Elem2"), ("3", "Elem3")] (Just "2")
        -- * UserWidget
        -- userWidget <- UI.mkSingle (Just "user_widget") Nothing (pure (C 0)) (pure (C 100)) $ UI.UserWidget userVal
        --
        let confBs = fillConf {UI.wconfHeight = Rpn "$min-height 10 +"}
        btns <- mconcat <$> mapM (UI.mkSingle confBs <=< UI.newButton "font-m" 16 . T.pack . show) [1..(5::Int)]
        let confCntn = UI.WidgetConfig (Just "menu") Nothing Nothing (Rpn "$width 140 -") (C 0) (C 100) (C 300)
        cnt2 <- (`UI.appendChild` btns) <$> UI.mkContainer confCntn UI.VerticalStack
        --
        clickableArea <- UI.mkSingle (fillConf {UI.wconfName = Just "clickable"}) =<< UI.newTransparent
        fill <- UI.mkSingle (fillConf {UI.wconfName = Just "fill"}) =<< UI.newFill
        --
        UI.prependRoot $ mconcat [clickableArea, label, button1, img, texfld, picker, fill, cnt2]
        -- * From file
        UI.appendRoot =<< UI.parseWidgetTree guiYaml

      let gui' = flip execState gui $ do
                   -- Modify attribute
                   modify' $ UI.update "clickable" (set (_1.ctxAttrib.clickable) True)
                   modify' $ UI.update "fill" (set (_1.ctxAttrib.visible) False)

      liftIO $ putStrLn $ UI.prettyWT $ UI.getWidgetTree gui'

      liftIO $ do
        let wt = UI.getWidgetTree gui'
            matchName (ctx,_) = ctx^.UI.ctxName == Just "number-labels"
        whenJust (UI.focusBy matchName $ UI.toZipper wt) $ \z1 -> do
          putStrLn $ UI.prettyWT $ fst z1
          whenJust (UI.goChild z1) $ putStrLn . UI.prettyWT . fst

      cursor <- UI.newCursor
      return (UITest gui' cursor, sdlAssets)
      where
        fillConf :: UI.WidgetConfig
        fillConf = def {UI.wconfWidth = Rpn "$width", UI.wconfHeight = Rpn "$height"}

    update :: Update (GameT IO) UITest
    update title0 = do
      es <- K.getEvents
      readyG . work =<< updateT es title0
      where
        updateT es t0 = do
          t1 <- t0 & tCursor %%~ UI.updateCursor es
          t1 & tGui %%~ UI.updateGui es (t1^.tCursor)
        readyG t = do
          liftIO $ do
            let es = UI.getGuiEvents $ t^.tGui
            unless (null es) $ do
              putStrLn "--- GuiEvents ---"
              mapM_ (putStrLn . ("* " ++) . show) es
              putStrLn "-----------------"
          g <- UI.readyRender $ t^.tGui
          return $ t & tGui .~ g

        work t =
          flip execState t $ do
            whenJust (UI.clickedOn UI.GuiActLeft "fill" gui) $ \_ -> do
              modGui $ UI.update "menu" $ set (_1.ctxAttrib.visible) False
              modGui $ UI.update "fill" $ set (_1.ctxAttrib.visible) False

            whenJust (UI.clickedOn UI.GuiActRight "clickable" gui) $ \pos -> do -- TODO: Right click
              modGui $ UI.update "menu" $ set (_1.ctxAttrib.visible) True
              modGui $ UI.setPositionInWorld "menu" pos
              modGui $ UI.update "fill" $ set (_1.ctxAttrib.visible) True

            -- -- ** Update title
            -- whenJust (UI.clickedOn UI.GuiActLeft "button" es) $ \_pos -> do
            --   modify' $
            --     over tCnt (+1)
            --   cnt <- gets $ view tCnt
            --   let title = T.pack $ show cnt
            --   modGui $ UI.update "label" (over _2 (UI.setTitle title))
          where
            gui = t^.tGui
            modGui = modify' . over tGui

    render :: Render (GameT IO) UITest
    render t = do
      when (UI.guiUpdated gui) $ do
        clearBy $ V4 0.97 0.97 0.97 1
        --
        -- K.printTest (V2 10 200) color "- Joysticks"
        -- vjs <- K.getJoysticks
        -- let showjs js = "#" <> T.pack (show (K.jsId js)) <> ": " <> K.jsDeviceName js
        -- V.imapM_ (\i js -> K.printTest (V2 10 (220 + i * 20)) color (showjs js)) vjs
        UI.render $ t^.tGui
        -- K.withRenderer $ \r -> do
        --   let P pos = t^.tCursor.cursorPos
        --   Prim.smoothCircle r pos 5 (V4 0 0 0 255) -- Cursor
        -- return ()
        -- where
        --   color = V3 50 50 50
      return $ UI.guiUpdated gui
      where
        gui = t^.tGui

    transit t = do
      when (isClicked nameMain) runTitleScene
      K.continue t
      where
        isClicked name = isJust $ UI.clickedOn UI.GuiActLeft name (t^.tGui)

clearBy :: MonadIO m => V4 Float -> m ()
clearBy (V4 r g b a) = liftIO $ do
  GL.clearColor $= GL.Color4 r g b a
  GL.clear [GL.ColorBuffer]
