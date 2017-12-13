{-# LANGUAGE RecordWildCards #-}
module Action where

import           Data.Maybe (mapMaybe)

import qualified SDL
import           SDL.Event

data Action
  = Select
  | Cancel
  deriving (Eq, Show, Read)

eventsToActions :: [SDL.EventPayload] -> [Action]
eventsToActions = mapMaybe conv
  where
    conv :: SDL.EventPayload -> Maybe Action
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
