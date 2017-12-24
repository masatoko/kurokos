{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Scene (runTitleScene) where

import           Control.Monad.Extra       (whenJust)
import qualified Data.ByteString           as BS
-- import           Control.Lens
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader
-- import           Control.Monad.State
-- import           Control.Monad.Trans.Class (lift)

import qualified Kurokos                   as K
import qualified Kurokos.Asset             as Asset
-- import qualified Kurokos.Asset.Raw            as Asset
import qualified Kurokos.Graphics          as G
import           Kurokos.Graphics.Vect
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
  , tEvents :: [(UI.GuiAction, UI.GuiEvent)]
  }

runTitleScene :: K.KurokosT (GameT IO) ()
runTitleScene = do
  colorScheme <- liftIO $ UI.readColorScheme "_data/gui-color-scheme.yaml"
  assets <- lift $ asks envAssets
  gui <- UI.newGui (UI.GuiEnv assets colorScheme) $
           UI.appendRoot =<< UI.parseWidgetTree =<< liftIO (BS.readFile "_data/gui-title.yaml")
  cursor <- UI.newCursor
  K.runScene scene $ Title gui cursor []
  where
    scene :: K.Scene Title (GameT IO) ()
    scene = K.Scene update render transit

    update t = do
      esSDL <- K.getEvents
      cursor <- UI.updateCursor esSDL $ tCursor t
      gui <- UI.updateGui esSDL cursor (tGui t)
      let es = UI.handleGui UI.defaultGuiHandler esSDL cursor gui
      gui' <- UI.readyRender gui
      return $ Title gui' cursor es

    render t = do
      liftIO $ do
        GL.clearColor $= GL.Color4 255 255 255 255
        GL.clear [GL.ColorBuffer]
      UI.render $ tGui t

    transit t = do
      whenJust (UI.clickedOn UI.GuiActLeft "start" es) $
        const runGameScene
      K.continue t
      where
        es = tEvents t
