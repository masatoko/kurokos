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
import           Foreign.C.Types           (CInt)
import           Linear.V2

import qualified SDL
import qualified SDL.Font                  as Font

import           Kurokos.GUI.Def           (RenderEnv (..))
import           Kurokos.GUI.Types         (Direction (..))
import           Kurokos.GUI.Widget
import           Kurokos.GUI.Widget.Render (createTextureFromWidget)

-- class Widget a where
--   showW :: a -> String
--   render :: (MonadIO m, MonadMask m) => RenderEnv m => a -> m ()

type Key = Int64
newtype SingleKey = SingleKey Key deriving Show
newtype ContainerKey = ContainerKey Key deriving Show

data WidgetTree
  = Single
      { singleKey :: SingleKey
      , wtTexture :: (V2 CInt, SDL.Texture)
      , wtWidget  :: Widget
      }
  | Container
      { containerKey :: ContainerKey
      , wtDir        :: Direction
      , wtChildren   :: [WidgetTree]
      }

instance Show WidgetTree where
  show (Single (SingleKey key) _ w)        = show key ++ show w
  show (Container (ContainerKey key) _ ws) = show key ++ show ws

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
    gui = GUI 0 1 (Container (ContainerKey 0) Vertical [])

genSingle :: (RenderEnv m, MonadIO m, MonadMask m) => Widget -> GuiT m WidgetTree
genSingle w = do
  tex <- lift $ createTextureFromWidget w
  key <- SingleKey <$> use gSCnt
  gSCnt += 1
  return $ Single key tex w

genContainer :: Monad m => Direction -> [WidgetTree] -> GuiT m WidgetTree
genContainer dir ws = do
  key <- ContainerKey <$> use gCCnt
  gCCnt += 1
  return $ Container key dir ws

putWT :: Monad m => WidgetTree -> GuiT m ()
putWT wt = gWTree .= wt

renderGUI :: (RenderEnv m, MonadIO m, MonadMask m) => GUI -> m ()
renderGUI g =
  void $ runStateT (go Vertical $ g^.gWTree) (pure 0)
  where
    go :: (RenderEnv m, MonadIO m, MonadMask m) => Direction -> WidgetTree -> StateT (V2 CInt) m ()
    go dir Single{..} = do
      pos <- get
      lift $ renderTexture tex $ SDL.Rectangle (SDL.P pos) size
      case dir of
        Vertical   -> put $ pos & _y +~ (size^._y)
        Horizontal -> put $ pos & _x +~ (size^._x)
      where
        (size, tex) = wtTexture
    go dir Container{..} = mapM_ (go wtDir) wtChildren
