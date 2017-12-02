module Kurokos.GUI
  ( module Import
  , GUI
  , GuiEnv (..)
  -- Def
  , RenderEnv (..)
  , HasEvent (..)
  --
  -- WContext (Lens)
  , ctxAttrib
  -- WidgetAttrib (Lens)
  , hoverable
  , clickable
  -- Types
  , ContainerType (..)
  , UExp (..)
  , Color
  , ColorSet (..)
  , WidgetPart (..)
  , WidgetColor (..)
  , WidgetColorModifier (..)
  -- Type
  , GuiEvent (..)
  , EventType (..)
  --
  , pretty
  , showTree
  , newGui
  , getWidgetTree
  , getGuiEvents
  -- , genSingle
  , genSingle
  , genContainer
  , appendRoot
  , prependRoot
  -- WidgetTree
  , append
  , prepend
  , appendC
  , prependC
  , appendChild
  , prependChild
  , wtappend
  , wtconcat
  , wtFromList
  -- Update
  , updateGui
  , readyRender
  -- Widget
  , update
  -- Rendering
  , render
  -- Helper
  , onClick
  ) where

import           Kurokos.GUI.Core
import           Kurokos.GUI.Update

import           Kurokos.GUI.Def
import           Kurokos.GUI.Event
import           Kurokos.GUI.Types
import           Kurokos.GUI.Widget.Make   as Import
import           Kurokos.GUI.Widget.Module as Import
import           Kurokos.GUI.WidgetTree
import           Kurokos.GUI.Helper
