{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Scene (runTitleScene) where

import           Control.Monad             (forM_, unless)
import           Control.Monad.Extra       (whenJust)
import qualified Data.ByteString           as BS
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader
-- import           Control.Lens
-- import           Control.Monad.State
-- import           Control.Monad.Trans.Class (lift)

import qualified Kurokos                   as K
-- import qualified Kurokos.Asset             as Asset
-- import qualified Kurokos.Asset.Raw            as Asset
import qualified Kurokos.Graphics          as G
-- import           Kurokos.Graphics.Vect
-- import           Kurokos.Graphics.Vect
import qualified Kurokos.UI                as UI

-- import qualified SDL
-- import           SDL.Event

import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Game
import           Scene.Game                (runGameScene)

data Title = Title
  { tGui    :: UI.GUI
  , tCursor :: UI.Cursor
  }

runTitleScene :: K.KurokosT (GameT IO) ()
runTitleScene = do
  stylemap <- liftIO $ UI.readStyleMap "_data/gui-stylemap.yaml"
  assets <- lift $ asks envAssets
  (_,gui) <- UI.newGui (UI.GuiEnv assets stylemap) $
               UI.appendRoot =<< UI.parseWidgetTree =<< liftIO (BS.readFile "_data/gui-title.yaml")
  cursor <- UI.newCursor
  K.runScene scene $ Title gui cursor
  where
    scene :: K.Scene Title (GameT IO) ()
    scene = K.Scene update render transit

    update t = do
      esSDL <- K.getEvents
      cursor <- UI.updateCursor esSDL $ tCursor t
      gui <- UI.updateGui esSDL cursor (tGui t)
      let es = UI.getGuiEvents gui
      unless (null es) $ liftIO $ do
        putStrLn "====="
        forM_ es $ \e -> putStrLn $ "- " ++ show e
      gui' <- UI.readyRender gui
      return $ Title gui' cursor

    render t = do
      when (UI.guiUpdated gui) $ do
        liftIO $ do
          GL.clearColor $= GL.Color4 255 255 255 255
          GL.clear [GL.ColorBuffer]
        G.withAlphaBlend $
          UI.render gui
      return $ UI.guiUpdated gui
      where
        gui = tGui t

    transit t = do
      whenJust (UI.clickedOn UI.GuiActLeft "start" gui) $
        const runGameScene
      K.continue t
      where
        gui = tGui t
