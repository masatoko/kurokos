{-# LANGUAGE DeriveDataTypeable #-}
module Kurokos.Exception where

import           Control.Exception as E
import           Data.Typeable

data KurokosException
  = UserExitException
  deriving (Show, Typeable)

instance Exception KurokosException
