module Kurokos.UI
  (
  -- * GUI
    GUI
  , GuiEnv (..)
  , GuiState
  , runGuiT
  , newGui
  , freeGui
  , getWidgetTree
  , setWidgetTree
  , modifyWidgetTree
  , getGuiEvents
  , guiUpdated
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
  , WidgetAttrib (..)
  , hoverable
  , clickable
  , draggable
  , droppable
  , visible
  -- * Style
  , Color
  , ContextStyle
  -- * Style scheme
  , StyleMap
  , readStyleMap
  , parseStyleMap
  , StyleConf (..)
  , StyleKey
  , StyleState (..)
  , makeStyleKey
  -- * Type
  , GuiEvent (..)
  , WidgetInfo (..)
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
  , mkTransparent
  , mkFill
  , mkLabel
  , newImageView
  , mkButton
  , mkSwitch
  , mkSlider
  , mkTextField
  , mkPicker
  -- * Widget.Module
  -- ** Getting
  , getBool
  , getInt
  , getFloat
  , getDouble
  , getText
  , getKey
  -- ** Setting
  , setText
  , setValueI
  -- * WidgetTree
  , WidgetTree
  , mkNull
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
  , modifyRoot
  , append
  , prepend
  , appendChild
  , prependChild
  , wtFromList
  -- * Zipper of WidgetTree
  , Zipper
  , goUnder
  , goChild
  , goOver
  , goUp
  , topMost
  , toZipper
  , fromZipper
  , focusBy
  -- * WidgetTree (WContext, Widget)
  , WTIdent
  , WTName
  , WTClass
  -- * Update GUI
  , updateGui
  , readyRender
  -- * Rendering GUI
  , render
  , renderWhenUpdated
  -- * Control
  , GuiHandler (..)
  , GuiAction (..)
  , defaultGuiHandler
  , topmostAt
  , topmostAtWith
  , modifyFocused
  , modifyWidget
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
  , isNameOf
  , isClassOf
  , isIdentOf
  , update
  , findByIdent
  , findByName
  , putChildToContainer
  , setPositionInWorld
  ) where

import           Kurokos.UI.Color
import           Kurokos.UI.Scheme
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
