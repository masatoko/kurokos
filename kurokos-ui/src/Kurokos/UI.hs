module Kurokos.UI
  (
  -- * GUI
    GUI
  , GuiEnv (..)
  , GuiState
  , newGui
  , freeGui
  , getWidgetTree
  -- * GuiT Monad
  , GuiT
  -- * Def
  , RenderEnv (..)
  , Renderable (..)
  -- * WContext
  , WContext
  , ctxIdent
  , ctxName
  , ctxAttrib
  , ctxWidgetState
  , ctxNeedsLayout
  , ctxNeedsRender
  -- * WidgetState
  , WidgetState
  , wstHover
  -- * WidgetAttrib
  , WidgetAttrib
  , hoverable
  , clickable
  , visible
  -- * Color
  , Color
  , WidgetColor (..)
  , wcBack
  , wcBorder
  , wcTitle
  , wcTint
  , ContextColor (..)
  -- * ColorScheme
  , ColorScheme
  , readColorScheme
  , parseColorScheme
  -- * Type
  , GuiEvent (..)
  , EventType (..)
  , UExp (..)
  -- * Widget
  , Widget (UserWidget)
  , newTransparent
  , newFill
  , newLabel
  , newImageView
  , newButton
  , newSwitch
  , newSlider
  -- * Widget.Module
  , isWidgetTrue
  -- * WidgetTree
  , WidgetTree
  , ContainerType (..)
  , pretty
  , showTree
  , readWidgetTree
  , parseWidgetTree
  , mkSingle
  , mkContainer
  , appendRoot
  , prependRoot
  , append
  , prepend
  , appendChild
  , prependChild
  , wtFromList
  -- * WidgetTree (WContext, Widget)
  , WTIdent
  , WTName
  -- * Update GUI
  , updateGui
  , readyRender
  -- * Rendering GUI
  , render
  -- * Control
  , GuiHandler (..)
  , GuiAction (..)
  , defaultGuiHandler
  , handleGui
  , clickByCursor
  , topmostAt
  -- * Cursor
  , Cursor
  , cursorPos
  , cursorArea
  , newCursor
  , updateCursor
  -- * Control.Helper
  , clickedOn
  -- * Helper
  , update
  , findByIdent
  , findByName
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
import           Kurokos.UI.File.Convert    (parseWidgetTree, readWidgetTree)
import           Kurokos.UI.Helper
import           Kurokos.UI.Types
import           Kurokos.UI.Update
import           Kurokos.UI.Widget
import           Kurokos.UI.Widget.Make
import           Kurokos.UI.Widget.Module
import           Kurokos.UI.WidgetTree
