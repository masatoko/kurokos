{-# LANGUAGE RecordWildCards #-}
module Kurokos.UI.Control.Control
  ( GuiHandler (..)
  , GuiAction (..)
  , defaultGuiHandler
  , handleGui
  --
  , clickByCursor
  , topmostAt
  , topmostAtWith
  ) where

import           Control.Lens
import           Data.Foldable             (toList)
import           Data.Maybe                (catMaybes, mapMaybe, maybeToList)
import           Linear.V2
import           Safe                      (lastMay)

import qualified SDL
import           SDL.Event

import           Kurokos.UI.Control.Cursor
import           Kurokos.UI.Core
import qualified Kurokos.UI.Event          as E
import           Kurokos.UI.Import
import           Kurokos.UI.Types
import           Kurokos.UI.Widget
import qualified Kurokos.UI.WidgetTree     as WT

data GuiAction
  = GuiActLeft
  | GuiActRight
  deriving (Eq, Show, Read)

data GuiHandler act = GuiHandler
  { ghClick :: SDL.EventPayload -> Maybe act
  }

defaultGuiHandler :: GuiHandler GuiAction
defaultGuiHandler = GuiHandler click
  where
    click (MouseButtonEvent MouseButtonEventData{..}) =
      if mouseButtonEventMotion == Pressed
        then
          case mouseButtonEventButton of
            ButtonLeft  -> Just GuiActLeft
            ButtonRight -> Just GuiActRight
        else Nothing
    click (KeyboardEvent KeyboardEventData{..})
      | pressed && keycode == SDL.KeycodeSpace  = Just GuiActLeft
      | pressed && keycode == SDL.KeycodeLShift = Just GuiActRight
      | otherwise                               = Nothing
      where
        keycode = SDL.keysymKeycode keyboardEventKeysym
        pressed = keyboardEventKeyMotion == Pressed
    click _ = Nothing

handleGui :: GuiHandler act -> [SDL.EventPayload] -> Cursor -> GUI -> [(act, E.GuiEvent)]
handleGui GuiHandler{..} esSDL cursor gui =
  case clickByCursor cursor gui of
    Just e  -> [(act, e) | act <- as]
    Nothing -> []
  where
    as = mapMaybe ghClick esSDL

-----

clickByCursor :: Cursor -> GUI -> Maybe E.GuiEvent
clickByCursor Cursor{..} gui = me
  where
    me = conv =<< wtTopmostAt _cursorPos isClickable (gui^.unGui._2.gstWTree)
      where
        isClickable = view (_1.ctxAttrib.clickable)
        conv (WContext{..}, w)
          | _ctxAttrib^.clickable = Just $ E.GuiEvent et w _ctxIdent _ctxName
          | otherwise             = Nothing
          where
            et = E.Clicked _cursorPos

topmostAt :: Point V2 CInt -> GUI -> Maybe (WContext, Widget)
topmostAt p gui = wtTopmostAt p (const True) (gui^.unGui._2.gstWTree)

-- | Same as `topmostAt` but with filtering function.
topmostAtWith :: Point V2 CInt -> (CtxWidget -> Bool) -> GUI -> Maybe (WContext, Widget)
topmostAtWith p isTarget gui = wtTopmostAt p isTarget (gui^.unGui._2.gstWTree)

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
