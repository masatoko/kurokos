module Kurokos.UI.Control.Control
  ( topmostAt
  , topmostAtWith
  , modifyFocused
  , controlLeft
  , controlRight
  -- * Internal
  , wtTopmostAt
  ) where

import           Control.Lens
import           Data.Foldable             (toList)
import           Data.Maybe                (catMaybes, mapMaybe, maybeToList)
import           Linear.V2
import           Safe                      (lastMay)

import qualified SDL
import           SDL.Event

import           Kurokos.UI.Control
import           Kurokos.UI.Control.Cursor
import           Kurokos.UI.Core
import qualified Kurokos.UI.Event          as E
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget
import           Kurokos.UI.Widget.Module  as WM
import qualified Kurokos.UI.WidgetTree     as WT

topmostAt :: Point V2 CInt -> GUI -> Maybe (WContext, Widget)
topmostAt p gui = wtTopmostAt p (const True) (gui^.unGui._2.gstWTree)

-- | Same as `topmostAt` but with filtering function.
topmostAtWith :: Point V2 CInt -> (CtxWidget -> Bool) -> GUI -> Maybe (WContext, Widget)
topmostAtWith p isTarget gui = wtTopmostAt p isTarget (gui^.unGui._2.gstWTree)

-- * Control

-- | Modify a focused widget
-- @
-- gui' = modifyFocused controlLeft gui
-- @
modifyFocused :: (CtxWidget -> CtxWidget) -> GUI -> GUI
modifyFocused f gui =
  gui & unGui._2.gstWTree %~ WT.wtModifyAt path (need . f)
  where
    path = gui^.unGui._2.gstFocus
    need = set (_1.ctxNeedsRender) True

controlLeft :: CtxWidget -> CtxWidget
controlLeft = over _2 WM.widgetLeft

controlRight :: CtxWidget -> CtxWidget
controlRight = over _2 WM.widgetRight

-- Internal

wtTopmostAt :: Point V2 CInt -> (CtxWidget -> Bool) -> GuiWidgetTree -> Maybe (WContext, Widget)
wtTopmostAt p isTarget = lastMay . wtFilterAt p isTarget

wtFilterAt :: Point V2 CInt -> (CtxWidget -> Bool) -> GuiWidgetTree -> [CtxWidget]
wtFilterAt aPos' isTarget = catMaybes . toList . fmap work
  where
    aPos = fromIntegral <$> aPos'

    work :: CtxWidget -> Maybe (WContext, Widget)
    work cw
      | vis && within && isTarget cw = Just cw
      | otherwise                    = Nothing
      where
        wst = cw^._1.ctxWidgetState
        pos = wst^.wstWorldPos
        size = wstSize wst
        --
        vis = wst^.wstVisible
        within = isWithinRect aPos pos size

isWithinRect :: (Num a, Ord a) => Point V2 a -> Point V2 a -> V2 a -> Bool
isWithinRect p p1 size =
  p1^._x <= px && px <= p2^._x && p1^._y <= py && py <= p2^._y
  where
    px = p^._x
    py = p^._y
    p2 = p1 + P size
