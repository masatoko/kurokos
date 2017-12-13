{-# LANGUAGE RecordWildCards #-}
module MyAction where

import           Data.Maybe (mapMaybe)

import qualified SDL
import           SDL.Event

data MyAction
  = Select
  | Cancel
  deriving (Eq, Show, Read)

eventsToMyActions :: [SDL.EventPayload] -> [MyAction]
eventsToMyActions = mapMaybe conv
  where
    conv :: SDL.EventPayload -> Maybe MyAction
    conv (KeyboardEvent KeyboardEventData{..})
      | pPressed && not keyboardEventRepeat =
          case SDL.keysymScancode keyboardEventKeysym of
            SDL.ScancodeSpace  -> Just Select
            SDL.ScancodeLShift -> Just Cancel
            _                  -> Nothing
      | otherwise = Nothing
      where
        pPressed = keyboardEventKeyMotion == SDL.Pressed
    conv _ = Nothing
