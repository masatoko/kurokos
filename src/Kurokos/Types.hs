{-# LANGUAGE RecordWildCards #-}
module Kurokos.Types
  ( RenderEnv
  , Font
  , Joystick (..)
  , openJoystickFromDevice
  , closeJoystick
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Exception.Safe as E
import           Data.Int (Int32)
import           Foreign.C.Types (CInt)
import           Linear.V2
import           Data.Text (Text)

import qualified SDL
import qualified SDL.Font
import qualified SDL.Raw.Haptic         as HAP
import           SDL.Raw.Types          (Haptic)
import           SDL.Internal.Types     (joystickPtr)

class RenderEnv m where
  getRenderer :: m SDL.Renderer
  getFont :: m SDL.Font.Font

type Font = SDL.Font.Font

data Joystick = Joystick
  { jsJoystick   :: !SDL.Joystick
  , jsId         :: !Int32
  , jsDeviceName :: !Text
  , jsDeviceId   :: !CInt
  , jsHap        :: !(Maybe Haptic)
  } deriving (Eq, Show)

openJoystickFromDevice :: MonadIO m => SDL.JoystickDevice -> m Joystick
openJoystickFromDevice device = do
  js <- SDL.openJoystick device
  jid <- SDL.getJoystickID js
  mHap <- liftIO $ getHaptic js
  return $ Joystick js jid dname did mHap
  where
    dname = SDL.joystickDeviceName device
    did = SDL.joystickDeviceId device

    getHaptic :: SDL.Joystick -> IO (Maybe Haptic)
    getHaptic j =
      (Just <$> HAP.hapticOpenFromJoystick (joystickPtr j)) `E.catch` handler
      where
        handler :: E.SomeException -> IO (Maybe Haptic)
        handler _ = return Nothing

closeJoystick :: MonadIO m => Joystick -> m ()
closeJoystick Joystick{..} = do
  mapM_ HAP.hapticClose jsHap
  SDL.closeJoystick jsJoystick
