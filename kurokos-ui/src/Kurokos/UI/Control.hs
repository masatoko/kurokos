{-# LANGUAGE RecordWildCards #-}
module Kurokos.UI.Control
  ( GuiHandler (..)
  , GuiAction (..)
  , defaultGuiHandler
  ) where

import qualified SDL
import           SDL.Event

data GuiAction
  = GuiActLeft
  | GuiActRight
  deriving (Eq, Show, Read)

data GuiHandler = GuiHandler
  { ghClick :: SDL.EventPayload -> Maybe GuiAction
  }

defaultGuiHandler :: GuiHandler
defaultGuiHandler = GuiHandler click
  where
    click (MouseButtonEvent MouseButtonEventData{..}) =
      if mouseButtonEventMotion == Pressed
        then
          case mouseButtonEventButton of
            ButtonLeft  -> Just GuiActLeft
            ButtonRight -> Just GuiActRight
            _           -> Nothing
        else Nothing
    click (KeyboardEvent KeyboardEventData{..})
      | pressed && keycode == SDL.KeycodeSpace  = Just GuiActLeft
      | pressed && keycode == SDL.KeycodeLShift = Just GuiActRight
      | otherwise                               = Nothing
      where
        keycode = SDL.keysymKeycode keyboardEventKeysym
        pressed = keyboardEventKeyMotion == Pressed
    click _ = Nothing
