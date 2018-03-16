{-# LANGUAGE OverloadedStrings #-}
module Kurokos.Text
  ( TextKey
  , VarKey
  , VarMap
  , TextSet
  , parseTextSet
  , getText
  , toListWith
  , toListWithM
  ) where

import           Kurokos.Text.Types
import           Kurokos.Text.Parse
