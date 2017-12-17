module Kurokos.UI
  ( module Import
  , GUI
  , GuiEnv (..)
  -- Def
  , RenderEnv (..)
  --
  -- WContext (Lens)
  , WContext
  , ctxIdent
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
  -- ** Color
  , Color
  , WidgetColor (..)
  , ContextColor (..)
  -- ** ColorScheme
  , ColorScheme
  , readColorScheme
  , parseColorScheme
  -- ** Type
  , GuiEvent (..)
  , EventType (..)
  --
  , pretty
  , showTree
  , newGui
  , freeGui
  , getWidgetTree
  -- GuiT
  , GuiT
  , modifyGui
  -- , getGuiEvents
  , newWidgetTreeFromData
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
  -- ** Control
  , GuiHandler (..)
  , GuiAction (..)
  , defaultGuiHandler
  , handleGui
  , clickByCursor
  , topmostAt
  -- ** Cursor
  , Cursor
  , cursorPos
  , cursorArea
  , newCursor
  , updateCursor
  -- ** Control.Helper
  , clickedOn
  -- ** Helper
  , update
  , glookup
  , setGlobalPosition
  ) where

import           Kurokos.UI.Color
import           Kurokos.UI.Color.Scheme    (ColorScheme, parseColorScheme,
                                             readColorScheme)
import           Kurokos.UI.Control.Control
import           Kurokos.UI.Control.Cursor
import           Kurokos.UI.Control.Helper  (clickedOn)
import           Kurokos.UI.Core
import           Kurokos.UI.Def
import           Kurokos.UI.Event
import           Kurokos.UI.File.Convert    (newWidgetTreeFromData)
import           Kurokos.UI.Helper
import           Kurokos.UI.Types
import           Kurokos.UI.Update
import           Kurokos.UI.Widget.Make     as Import
import           Kurokos.UI.Widget.Module   as Import
import           Kurokos.UI.WidgetTree
