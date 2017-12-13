{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Game where

import           Control.Exception.Safe       (MonadThrow)
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource

import qualified Kurokos.Asset.SDL            as Asset

import           Kurokos                      (KurokosT)

data GameEnv = GameEnv
  { envAssets :: Asset.SDLAssetManager
  }

data GameState = GameState

newtype GameT m a = GameT {
    runGT :: ReaderT GameEnv (StateT GameState m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader GameEnv, MonadState GameState, MonadThrow, MonadBase base)

runGameT :: GameEnv -> GameState -> GameT m a -> m (a, GameState)
runGameT env st k =
  runStateT (runReaderT (runGT k) env) st

instance MonadThrow (KurokosT (GameT IO)) where
  throwM e = liftIO $ throwM e

instance MonadTrans GameT where
  lift = GameT . lift . lift

instance MonadTransControl GameT where
  type StT GameT a = (a, GameState)
  liftWith f = GameT $
    liftWith $ \runS ->
      liftWith $ \runR ->
        f $ \ma -> runR $ runS $ runGT ma
  restoreT = GameT . restoreT . restoreT

instance MonadBaseControl base m => MonadBaseControl base (GameT m) where
  type StM (GameT m) a = ComposeSt GameT m a
  liftBaseWith         = defaultLiftBaseWith
  restoreM             = defaultRestoreM
