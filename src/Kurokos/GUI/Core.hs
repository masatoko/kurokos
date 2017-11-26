{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
module Kurokos.GUI.Core where

import Control.Lens
import Control.Monad.State
import Data.Text (Text)
import Data.Int (Int64)

import Kurokos.GUI.Def

-- data Direction
--   = DirH -- Horizontal
--   | DirV -- Vertical
--   deriving Show


type Key = Int64
newtype SingleKey = SingleKey Key deriving Show
newtype ContainerKey = ContainerKey Key deriving Show

data WidgetTree
  = forall a. (Widget a)
  => Single SingleKey a | Container ContainerKey [WidgetTree]

instance Show WidgetTree where
  show (Single (SingleKey key) a) = show key ++ "<" ++ showW a ++ ">"
  show (Container (ContainerKey key) ws) = show key ++ "@" ++ show ws

data GuiState = GuiState
  { _gsSCnt :: Key
  , _gsCCnt :: Key
  --
  , _gsWTree :: WidgetTree
  } deriving Show

makeLenses ''GuiState

getWidgetTree :: GuiState -> WidgetTree
getWidgetTree = _gsWTree

-- data GuiT m a = GuiT {
--     runGT :: StateT GuiState m a
--   } deriving (Functor, Applicative, Monad, MonadIO, MonadState GuiState)

-- runGuiT :: Monad m => GuiState -> GuiT m a -> m GuiState
-- runGuiT gst k = execStateT (runGT k) gst

type GuiT m a = StateT GuiState m a

runGuiT :: Monad m => GuiState -> GuiT m a -> m GuiState
runGuiT gst k = execStateT k gst

newGui :: Monad m => GuiT m () -> m GuiState
newGui = runGuiT gs0
  where
    gs0 = GuiState 0 1 (Container (ContainerKey 0) [])

genSingle :: (Widget a, Monad m) => a -> GuiT m WidgetTree
genSingle a = do
  key <- SingleKey <$> use gsSCnt
  gsSCnt += 1
  return $ Single key a

genContainer :: Monad m => [WidgetTree] -> GuiT m WidgetTree
genContainer ws = do
  key <- ContainerKey <$> use gsCCnt
  gsCCnt += 1
  return $ Container key ws

putWT :: Monad m => WidgetTree -> GuiT m ()
putWT wt = gsWTree .= wt
