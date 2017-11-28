module Kurokos.GUI
  ( GUI
  , GuiEnv (..)
  , Direction (..)
  , UExp (..)
  --
  , RenderEnv (..)
  , HasEvent (..)
  --
  , newGui
  , genSingle
  , genContainer
  , prependRoot
  , prependRootWs
  --
  , update
  , render
  -- Make
  , newLabel
  ) where

import Kurokos.GUI.Core

import Kurokos.GUI.Def
import Kurokos.GUI.Types
import Kurokos.GUI.Widget.Make
