{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Scene.Game where

import Control.Monad.State
import           Control.Lens
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Reader
import           Control.Monad.Trans.Class    (lift)

import qualified Kurokos                      as K
import qualified Kurokos.Asset                as Asset
-- import qualified Kurokos.Asset.Raw            as Asset
import qualified Kurokos.Graphics             as G
import           Kurokos.Graphics.Vect
-- import           Kurokos.Graphics.Vect
-- import qualified Kurokos.UI                   as UI

import qualified SDL
import           SDL.Event

import           Graphics.Rendering.OpenGL    (($=))
import qualified Graphics.Rendering.OpenGL    as GL

import           Game

data BirdState = BirdState
  { _y  :: Double
  , _vy :: Double
  }

makeLenses ''BirdState

runGameScene :: K.KurokosT (GameT IO) ()
runGameScene = do
  ast <- lift $ asks envAssets
  let Just bird = Asset.lookupTexture "bird-image" ast
  K.runScene (K.Scene update (render bird) transit) $ BirdState 0 0
  where
    update mst = do
      es <- K.getEvents
      flip execStateT mst $ do
        vy += 0.1
        vy' <- use vy
        y += vy'
        when (any flapped es) $ vy -= 3
        vy %= max (-10) . min 10
      where
        flapped (KeyboardEvent KeyboardEventData{..}) =
          SDL.keysymScancode keyboardEventKeysym == SDL.ScancodeSpace
            && keyboardEventKeyMotion == Pressed
            && not keyboardEventRepeat
        flapped _ = False

    render bird mst = do
      r <- K.getRenderer
      liftIO $ do
        GL.clearColor $= bgColor
        GL.clear [GL.ColorBuffer]
        kst <- SDL.getKeyboardState
        let rect = if kst SDL.ScancodeSpace then rect2 else rect1
        G.renderTexture r bird rect rctx
      return True
      where
        bgColor = GL.Color4 r g b a
          where
            V4 r g b a = V4 20 20 20 255 <&> (/ 255)

        size = V2 16 16
        rect1 = Just (P (V2 0 0), size)
        rect2 = Just (P (V2 16 0), size)

        y' = round $ mst^.y
        rctx = G.RContext (V2 200 y') (pure 64) Nothing Nothing

    transit mst
      | mst^.y > 480 = K.end ()
      | otherwise    = K.continue mst
