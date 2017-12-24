# Boilderplates

## Scene

```haskell
import qualified Kurokos               as K
import qualified Kurokos.Graphics      as G
import           Kurokos.Graphics.Vect

import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

data Game = Game

runMyScene :: K.KurokosT (GameT IO) ()
runMyScene =
  K.runScene scene Game
  where
    scene :: K.Scene Game (GameT IO) ()
    scene = K.Scene update render transit

    update = return

    render _ =
      liftIO $ do
        GL.clearColor $= GL.Color4 0 0 0 255
        GL.clear [GL.ColorBuffer]

    transit =
      K.continue
```
