module Kurokos.GUI
  ( module Import
  , GUI
  , GuiEnv (..)
  -- Def
  , RenderEnv (..)
  , HasEvent (..)
  --
  -- WContext (Lens)
  , ctxWidgetState
  , ctxAttrib
  -- WidgetState (Lens)
  , wstHover
  -- WidgetAttrib (Lens)
  , hoverable
  , clickable
  , visible
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
  , appendChild
  , prependChild
  , wtFromList
  -- Update
  , updateGui
  , readyRender
  -- Rendering
  , render
  -- Helper
  , clicked
  , update
  , glookup
  , setGlobalPosition
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
