module Scene.Game where

import           Control.Monad.IO.Class    (liftIO)

import qualified Kurokos                   as K
import qualified Kurokos.Asset             as Asset
import qualified Kurokos.Asset.Raw         as Asset
import qualified Kurokos.Graphics          as G
import           Kurokos.Graphics.Vect
-- import qualified Kurokos.UI                   as UI

import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Game

data MyGame = MyGame

runMainScene :: K.KurokosT (GameT IO) ()
runMainScene =
  K.runScene scene MyGame
  where
    scene :: K.Scene MyGame (GameT IO) ()
    scene = K.Scene update render transit

    update = return

    render _ =
      liftIO $ do
        GL.clearColor $= GL.Color4 0 0 0 255
        GL.clear [GL.ColorBuffer]

    transit =
      K.continue
