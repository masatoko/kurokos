{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Kurokos.GUI.Core where

import           Control.Exception.Safe    (MonadMask)
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Int                  (Int64)
import           Data.Text                 (Text)
import           Linear.V2                 (V2 (..))

import qualified SDL
import qualified SDL.Font                  as Font

import           Kurokos.GUI.Def           (RenderEnv (..))
import           Kurokos.GUI.Widget
import           Kurokos.GUI.Widget.Render (createTextureFromWidget)

-- data Direction
--   = DirH -- Horizontal
--   | DirV -- Vertical
--   deriving Show

-- class Widget a where
--   showW :: a -> String
--   render :: (MonadIO m, MonadMask m) => RenderEnv m => a -> m ()

type Key = Int64
newtype SingleKey = SingleKey Key deriving Show
newtype ContainerKey = ContainerKey Key deriving Show

data WidgetTree
  = Single
      { singleKey :: SingleKey
      , wtTexture :: SDL.Texture
      , wtWidget  :: Widget
      }
  | Container
      { containerKey :: ContainerKey
      , wtChildren   :: [WidgetTree]
      }

instance Show WidgetTree where
  show (Single (SingleKey key) _ w)      = show key ++ show w
  show (Container (ContainerKey key) ws) = show key ++ show ws

newtype GuiEnv = GuiEnv
  { geFont :: Font.Font
  }

data GUI = GUI
  { _gSCnt  :: Key
  , _gCCnt  :: Key
  --
  , _gWTree :: WidgetTree
  } deriving Show

makeLenses ''GUI

getWidgetTree :: GUI -> WidgetTree
getWidgetTree = _gWTree

newtype GuiT m a = GuiT {
    runGT :: ReaderT GuiEnv (StateT GUI m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader GuiEnv, MonadState GUI)

runGuiT :: Monad m => GuiEnv -> GUI -> GuiT m a -> m GUI
runGuiT env g k = execStateT (runReaderT (runGT k) env) g

instance MonadTrans GuiT where
  lift = GuiT . lift . lift

newGui :: Monad m => GuiEnv -> GuiT m () -> m GUI
newGui env = runGuiT env gui
  where
    gui = GUI 0 1 (Container (ContainerKey 0) [])

genSingle :: (RenderEnv m, MonadIO m, MonadMask m) => Widget -> GuiT m WidgetTree
genSingle w = do
  tex <- lift $ createTextureFromWidget w
  key <- SingleKey <$> use gSCnt
  gSCnt += 1
  return $ Single key tex w

genContainer :: Monad m => [WidgetTree] -> GuiT m WidgetTree
genContainer ws = do
  key <- ContainerKey <$> use gCCnt
  gCCnt += 1
  return $ Container key ws

putWT :: Monad m => WidgetTree -> GuiT m ()
putWT wt = gWTree .= wt

renderGUI :: (RenderEnv m, MonadIO m, MonadMask m) => GUI -> m ()
renderGUI g = go $ g^.gWTree
  where
    go Single{..} =
      renderTexture wtTexture $ SDL.Rectangle (SDL.P $ V2 0 0) (V2 50 50)
    go Container{..} = mapM_ go wtChildren
