{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import qualified SDL
import qualified SDL.Font                  as Font

import           Kurokos.GUI.Def           (RenderEnv)
import           Kurokos.GUI.Widget
import           Kurokos.GUI.Widget.Render (renderWidget)

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
  = Single SingleKey Widget
  | Container ContainerKey [WidgetTree]
  deriving Show

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

newGui :: Monad m => GuiEnv -> GuiT m () -> m GUI
newGui env = runGuiT env gui
  where
    gui = GUI 0 1 (Container (ContainerKey 0) [])

genSingle :: Monad m => Widget -> GuiT m WidgetTree
genSingle a = do
  key <- SingleKey <$> use gSCnt
  gSCnt += 1
  return $ Single key a

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
    go (Single _ a)     = renderWidget a
    go (Container _ ws) = mapM_ go ws
