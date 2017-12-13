module Kurokos.UI.Import
  (
    MonadIO
  , MonadResource
  , allocate
  , lift
  , liftIO
  , CInt
  , Int32
  , V2 (..)
  , V4 (..)
  , when
  , unless
  , asks
  , Rectangle (..)
  , Point (..)
  --
  , RenderEnv (..)
  -- Maybe
  , fromMaybe
  ) where

import           Control.Monad              (unless, when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader.Class (asks)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Resource (MonadResource, allocate)
import           Data.Int                   (Int32)
import           Data.Maybe
import           Foreign.C.Types            (CInt)
import           Linear.V2
import           Linear.V4

import           SDL

import           Kurokos.UI.Def
