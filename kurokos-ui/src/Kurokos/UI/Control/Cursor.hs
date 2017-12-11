{-# LANGUAGE RecordWildCards #-}
module Kurokos.UI.Control.Cursor
  ( Cursor (..)
  , newCursor
  , updateCursor
  ) where

import           Control.Lens
import           Data.List         (foldl')

import           Kurokos.UI.Import
import           Kurokos.UI.Types

import qualified SDL
import           SDL.Event

newCursor :: (MonadIO m, RenderEnv m) => m Cursor
newCursor = do
  winSize <- getWindowSize
  let area = SDL.Rectangle (pure 0) winSize
  return $ Cursor (pure 0) area

updateCursor :: (MonadIO m, RenderEnv m) => [SDL.EventPayload] -> Cursor -> m Cursor
updateCursor es cursor0 = do
  locmode <- SDL.getMouseLocationMode
  let isAbs = locmode == SDL.AbsoluteLocation
  return $ foldl' (work isAbs) cursor0 es
  where
    SDL.Rectangle (P (V2 xmin ymin)) (V2 xmax ymax) = cursor0^.cursorArea
    work isAbs c (MouseMotionEvent MouseMotionEventData{..}) =
      if isAbs
        then c & cursorPos .~ (fromIntegral <$> mouseMotionEventPos)
        else c & cursorPos %~ trim . modPos
      where
        modPos pos =
          pos + P (fromIntegral <$> mouseMotionEventRelMotion)

        trim (P (V2 x y)) = P $ V2 x' y'
          where
            x' = max xmin . min xmax $ x
            y' = max ymin . min ymax $ y
    work _ c _ = c
