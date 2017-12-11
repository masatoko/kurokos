{-# LANGUAGE RecordWildCards #-}
module Kurokos.UI.Cursor
  ( Cursor (..)
  , makeCursor
  , updateCursor
  ) where

import           Control.Lens
import           Data.List         (foldl')

import           Kurokos.UI.Import
import           Kurokos.UI.Types

import qualified SDL
import           SDL.Event

updateCursor :: MonadIO m => [SDL.EventPayload] -> Cursor -> m Cursor
updateCursor es cursor0 = do
  locmode <- SDL.getMouseLocationMode
  let isAbs = locmode == SDL.AbsoluteLocation
  return $ foldl' (work isAbs) cursor0 es
  where
    work isAbs c (MouseMotionEvent MouseMotionEventData{..}) =
      if isAbs
        then c & cursorPos .~ (fromIntegral <$> mouseMotionEventPos)
        else c & cursorPos %~ trim . modPos
      where
        modPos pos =
          pos + P (fromIntegral <$> mouseMotionEventRelMotion)

        trim (P (V2 x y)) = P $ V2 x' y'
          where
            x' = max 0 . min 640 $ x
            y' = max 0 . min 480 $ y
    work _ c _ = c
