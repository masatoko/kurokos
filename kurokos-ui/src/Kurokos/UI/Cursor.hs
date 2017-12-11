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

updateCursor :: (MonadIO m, RenderEnv m) => [SDL.EventPayload] -> Cursor -> m Cursor
updateCursor es cursor0 = do
  winSize <- getWindowSize
  locmode <- SDL.getMouseLocationMode
  let isAbs = locmode == SDL.AbsoluteLocation
  return $ foldl' (work isAbs winSize) cursor0 es
  where
    work isAbs (V2 w h) c (MouseMotionEvent MouseMotionEventData{..}) =
      if isAbs
        then c & cursorPos .~ (fromIntegral <$> mouseMotionEventPos)
        else c & cursorPos %~ trim . modPos
      where
        modPos pos =
          pos + P (fromIntegral <$> mouseMotionEventRelMotion)

        trim (P (V2 x y)) = P $ V2 x' y'
          where
            x' = max 0 . min w $ x
            y' = max 0 . min h $ y
    work _ _ c _ = c
