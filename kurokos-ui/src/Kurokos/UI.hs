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
  -- * Type
  , CtxWidget
  , GuiWidgetTree
  -- * WContext
  , WContext
  , ctxIdent
  , ctxName
  , ctxContainerType
  , ctxAttrib
  , ctxWidgetState
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
  -- * Style
  , Style (..)
  , styleTextAlign
  , styleMargin
  , TextAlign (..)
  , LRTB (..)
  -- * Widget
  , Widget (UserWidget)
  , Value (..)
  , newTransparent
  , newFill
  , newLabel
  , newImageView
  , newButton
  , newSwitch
  , newSlider
  -- * Widget.Module
  -- ** Getting
  , getBool
  , getInt
  , getFloat
  , getDouble
  -- * WidgetTree
  , WidgetTree
  , ContainerType (..)
  , pretty
  , prettyWith
  , showTree
  , readWidgetTree
  , parseWidgetTree
  , WidgetConfig (..)
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
  , topmostAtWith
  -- * Cursor
  , Cursor
  , cursorPos
  , cursorArea
  , newCursor
  , updateCursor
  -- * Control.Helper
  , clickedOn
  -- * Helper
  , prettyWT
  , update
  , findByIdent
  , findByName
  , setPositionInWorld
  ) where

import           Kurokos.UI.Color
import           Kurokos.UI.Color.Scheme    (ColorScheme, parseColorScheme,
                                             readColorScheme)
import           Kurokos.UI.Control
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
