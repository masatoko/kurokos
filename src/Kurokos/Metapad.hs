module Kurokos.Metapad
  ( Input
  , Joystick
  , Metapad
  , MouseButton (..)
  , InputMotion (..)
  , HatDir (..)
  , metapadFromList
  , addAction
  , makeActions
  , newJoystickAt
  , freeJoystick
  -- Helper
  , hold, pressed, released
  -- Joystick
  , monitorJoystick
  , numAxes, axisPosition
  , joyHold, joyPressed, joyReleased
  , joyAxis, joyAxis2
  , joyAxisChanged, joyAxisChanged2
  -- , joyAllButtons, joyAllAxes
  -- Hat
  , joyHat
  -- , joyAllHat
  -- Mouse
  , mousePosAct
  , mouseMotionAct
  , mouseButtonAct
  , mouseWheelAct
  , touchMotionAct
  -- Haptic
  , rumble
  ) where

import qualified Control.Exception      as E
import           Control.Monad          (forM_, join)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Bits              (testBit)
import           Data.Int               (Int16, Int32)
import qualified Data.Map               as M
import           Data.Maybe             (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Vector            as V
import           Data.Word              (Word32, Word8)
import           Foreign.C.Types        (CInt)
import           Linear.Affine
import           Linear.V2
import           Safe                   (headMay)

import qualified SDL
import           SDL.Internal.Types     (joystickPtr)
import qualified SDL.Raw.Haptic         as HAP
import           SDL.Raw.Types          (Haptic)

import           Kurokos.Types          (PadId)

type Interpreter action = Input -> IO (Maybe (PadId, action))

newtype Metapad action = Metapad [Interpreter action]

instance Monoid (Metapad a) where
  mempty = Metapad []
  mappend (Metapad xs) (Metapad ys) = Metapad $ xs ++ ys

metapadFromList :: [Interpreter a] -> Metapad a
metapadFromList = Metapad

addAction :: Interpreter a -> Metapad a -> Metapad a
addAction f (Metapad fs) = Metapad $ f:fs

data Input = Input
  { keyboard      :: [SDL.KeyboardEventData]
  , mouseMotion   :: [SDL.MouseMotionEventData]
  , mouseButton   :: [SDL.MouseButtonEventData]
  , mouseWheel    :: [SDL.MouseWheelEventData]
  , joyButtons    :: [SDL.JoyButtonEventData]
  , joyAxes       :: [SDL.JoyAxisEventData]
  , touches       :: [SDL.TouchFingerEventData]
  , touchMotions  :: [SDL.TouchFingerMotionEventData]
  , joyAxesCurPos :: M.Map Word8 Int16
  , joyAxesPrePos :: M.Map Word8 Int16
  , curHat        :: !SDL.JoyHatPosition -- TODO: Should identify joystick id and hat index
  , preHat        :: !SDL.JoyHatPosition
  , modState      :: !SDL.KeyModifier
  , keyState      :: SDL.Scancode -> Bool
  , mousePos      :: Point V2 CInt
  , mouseButtons  :: SDL.MouseButton -> Bool
  }

data MouseButton
  = ButtonLeft
  | ButtonRight
  deriving (Show, Read, Eq, Ord)

data InputMotion
  = Pressed
  | Released
  | Holded
  deriving (Show, Read, Eq, Ord)

data HatDir
  = HDUp
  | HDRight
  | HDDown
  | HDLeft
  deriving (Show, Read, Eq, Ord, Enum)

snapshotInput :: MonadIO m => Maybe Input -> [SDL.Event] -> m Input
snapshotInput mPreInput es =
  Input (sel kb) (sel mm) (sel mb) (sel mw)
        (sel jb) jaes (sel td) (sel tm)
        curAxesPos preAxesPos
        curHat' preHat'
        <$> SDL.getModState
        <*> SDL.getKeyboardState
        <*> SDL.getAbsoluteMouseLocation
        <*> SDL.getMouseButtons
  where
    es' = map SDL.eventPayload es
    sel f = mapMaybe f es'
    -- Keyboard
    kb (SDL.KeyboardEvent d) = Just d
    kb _                     = Nothing
    mm (SDL.MouseMotionEvent d) = Just d
    mm _                        = Nothing
    mb (SDL.MouseButtonEvent d) = Just d
    mb _                        = Nothing
    mw (SDL.MouseWheelEvent d) = Just d
    mw _                       = Nothing
    -- Joystick
    jb (SDL.JoyButtonEvent d) = Just d
    jb _                      = Nothing
    ja (SDL.JoyAxisEvent d) = Just d
    ja _                    = Nothing
    jaes = sel ja
    -- Touch
    td (SDL.TouchFingerEvent d) = Just d
    td _                        = Nothing
    tm (SDL.TouchFingerMotionEvent d) = Just d
    tm _                              = Nothing
    --
    preHat' = fromMaybe SDL.HatCentered $ curHat <$> mPreInput
    curHat' = fromMaybe preHat' $ headMay $ sel hat
      where
        hat (SDL.JoyHatEvent d) = Just $ SDL.joyHatEventValue d
        hat _                   = Nothing
    --
    preAxesPos =
      case mPreInput of
        Just pre -> joyAxesCurPos pre
        Nothing  -> M.empty
    curAxesPos =
      case mPreInput of
        Just pre -> M.fromList (map work jaes) `M.union` joyAxesCurPos pre
        Nothing  -> M.empty
      where
        work jaed = (a,v)
          where
            a = SDL.joyAxisEventAxis jaed
            v = SDL.joyAxisEventValue jaed

makeActions :: MonadIO m => Maybe Input -> [SDL.Event] -> Metapad a -> m ([(PadId, a)], Input)
makeActions mPreInput es (Metapad fs) =
  liftIO $ do
    i <- snapshotInput mPreInput es
    as <- catMaybes <$> mapM (\f -> f i) fs
    return (as, i)

-- * Helper

hold :: SDL.Scancode -> PadId -> act -> Interpreter act
hold code pid act i =
  return $ boolToMaybe (pid,act) $ keyState i code

pressed :: SDL.Scancode -> PadId -> act -> Interpreter act
pressed code pid act i =
  return $ boolToMaybe (pid,act) $ any (isTargetKey code SDL.Pressed) $ keyboard i

released :: SDL.Scancode -> PadId -> act -> Interpreter act
released code pid act i =
  return $ boolToMaybe (pid,act) $ any (isTargetKey code SDL.Released) $ keyboard i

isTargetKey :: SDL.Scancode -> SDL.InputMotion -> SDL.KeyboardEventData -> Bool
isTargetKey code motion e =
  let isCode = (SDL.keysymScancode . SDL.keyboardEventKeysym) e == code
      notRep = not (SDL.keyboardEventRepeat e)
      isPressed = SDL.keyboardEventKeyMotion e == motion
  in notRep && isCode && isPressed

-- Mouse

mousePosAct :: Integral a => (V2 a -> act) -> Interpreter act
mousePosAct f i = return . Just . (,) 0 . f $ fromIntegral <$> pos
  where (P pos) = mousePos i

mouseMotionAct :: (V2 Int32 -> act) -> Interpreter act
mouseMotionAct mk input =
  return $ (,) 0 . mk . SDL.mouseMotionEventRelMotion <$> headMay es
  where
    es = mouseMotion input

mouseButtonAct :: MouseButton -> InputMotion -> PadId -> act -> Interpreter act
mouseButtonAct prtBtn prtMotion pid act i = return $
  case prtMotion of
    Holded -> boolToMaybe (pid,act) $ mouseButtons i btn
    _      -> boolToMaybe (pid,act) $ any isTarget $ mouseButton i
  where
    btn = case prtBtn of
            ButtonLeft  -> SDL.ButtonLeft
            ButtonRight -> SDL.ButtonRight
    motion = case prtMotion of
               Pressed  -> SDL.Pressed
               Released -> SDL.Released
               _        -> error "@mouseButtonAct"
    isTarget e =
      SDL.mouseButtonEventButton e == btn
        && SDL.mouseButtonEventMotion e == motion

mouseWheelAct :: (V2 Int32 -> act) -> Interpreter act
mouseWheelAct mk input =
  return $ (,) 0 . mk . SDL.mouseWheelEventPos <$> headMay es
  where
    es = mouseWheel input

-- Touch

touchMotionAct :: (V2 Double -> act) -> Interpreter act
touchMotionAct mk input =
  return $ (,) 0 . mk . fmap realToFrac . SDL.touchFingerMotionEventRelMotion <$> headMay es
  where
    es = touchMotions input

-- Joystick

type JoystickID = Int32

data Joystick = Joy
  { js    :: !SDL.Joystick
  , jsId  :: !JoystickID
  , jsHap :: !(Maybe Haptic)
  } deriving (Eq, Show)

joyToPadId :: Joystick -> PadId
joyToPadId = fromIntegral . jsId

newJoystickAt :: MonadIO m => Int -> m (Maybe Joystick)
newJoystickAt i = do
  ds <- SDL.availableJoysticks
  liftIO $ case ds V.!? i of
    Nothing  -> return Nothing
    Just dev -> Just <$> (makeJoystick =<< SDL.openJoystick dev)
  where
    makeJoystick :: MonadIO m => SDL.Joystick -> m Joystick
    makeJoystick j = do
      mHap <- liftIO getHaptic
      mapM_ HAP.hapticRumbleInit mHap
      Joy j <$> SDL.getJoystickID j <*> pure mHap
      where
        getHaptic :: IO (Maybe Haptic)
        getHaptic =
          (Just <$> HAP.hapticOpenFromJoystick (joystickPtr j)) `E.catch` handler

        handler :: E.SomeException -> IO (Maybe Haptic)
        handler _e =
          -- putStrLn $ "Exception @getHaptic: " ++ show e
          return Nothing

freeJoystick :: MonadIO m => Joystick -> m ()
freeJoystick joy = do
  mapM_ HAP.hapticClose $ jsHap joy
  SDL.closeJoystick $ js joy

-- for test
monitorJoystick :: Joystick -> IO ()
monitorJoystick joy = do
  n <- fromIntegral <$> SDL.numAxes j
  mapM_ work $ take n [0..]
  where
    j = js joy
    work i = (putStrLn . prog i) =<< SDL.axisPosition j i

    norm :: Int16 -> Double
    norm v = fromIntegral v / 32768

    prog :: CInt -> Int16 -> String
    prog i a = show i ++ ": " ++ p ++ " ... " ++ show v
      where
        v =  norm a
        deg = truncate $ (v + 1) * 10
        p = take 20 $ replicate deg '*' ++ repeat '-'

numAxes :: Joystick -> IO Int
numAxes joy = fromIntegral <$> SDL.numAxes (js joy)

axisPosition :: Joystick -> Word8 -> IO Int16
axisPosition joy idx = SDL.axisPosition (js joy) (fromIntegral idx)

joyHold :: Joystick -> Word8 -> act -> Interpreter act
joyHold joy button act _ = do
  p <- liftIO $ SDL.buttonPressed (js joy) (fromIntegral button)
  return $ boolToMaybe (joyToPadId joy, act) p

joyPressed :: Joystick -> Word8 -> act -> Interpreter act
joyPressed joy button act i =
  return $ boolToMaybe (joyToPadId joy, act) $ any (isTargetButton joy button SDL.JoyButtonPressed) $ joyButtons i

joyReleased :: Joystick -> Word8 -> act -> Interpreter act
joyReleased joy button act i =
  return $ boolToMaybe (joyToPadId joy, act) $ any (isTargetButton joy button SDL.JoyButtonReleased) $ joyButtons i

isTargetButton :: Joystick -> Word8 -> SDL.JoyButtonState -> SDL.JoyButtonEventData -> Bool
isTargetButton joy button state e =
  isId && isButton && isState
  where
    isId = SDL.joyButtonEventWhich e == jsId joy
    isButton = SDL.joyButtonEventButton e == button
    isState = SDL.joyButtonEventState e == state

joyAxis :: Joystick -> Word8 -> (Int16 -> Int16 -> Maybe act) -> Interpreter act
joyAxis joy axis make i = return . fmap ((,) pid) . join $ mmAct
  where
    pid = joyToPadId joy
    mmAct = make <$> M.lookup axis (joyAxesPrePos i)
                 <*> M.lookup axis (joyAxesCurPos i)

joyAxis2 :: Joystick -> Word8 -> Word8 -> (Int16 -> Int16 -> act) -> Interpreter act
joyAxis2 joy a0 a1 make _ = fmap (Just . (,) pid) $
  make <$> SDL.axisPosition (js joy) (fromIntegral a0)
       <*> SDL.axisPosition (js joy) (fromIntegral a1)
  where
    pid = joyToPadId joy

joyAxisChanged :: Joystick -> Word8 -> (Int16 -> Int16 -> Maybe act) -> Interpreter act
joyAxisChanged joy axis make i = return $ (,) pid <$> mAct
  where
    pid = joyToPadId joy
    mAct = do
      cur <- headMay . mapMaybe (axisValue joy axis) . joyAxes $ i
      pre <- M.lookup axis $ joyAxesPrePos i
      make pre cur

joyAxisChanged2 :: Joystick -> Word8 -> Word8 -> (Int16 -> Int16 -> act) -> Interpreter act
joyAxisChanged2 joy a0 a1 make i = fmap ((,) pid) <$>
  work (headMay . mapMaybe (axisValue joy a0) . joyAxes $ i)
       (headMay . mapMaybe (axisValue joy a1) . joyAxes $ i)
  where
    pid = joyToPadId joy
    work (Just v0) (Just v1) = return . Just $ make v0 v1
    work (Just v0) Nothing   = fmap Just $ make <$> pure v0 <*> SDL.axisPosition (js joy) (fromIntegral a1)
    work Nothing   (Just v1) = fmap Just $ make <$> SDL.axisPosition (js joy) (fromIntegral a0) <*> pure v1
    work Nothing   Nothing   = return Nothing

axisValue :: Joystick -> Word8 -> SDL.JoyAxisEventData -> Maybe Int16
axisValue joy axis (SDL.JoyAxisEventData jid' axis' v)
  | jsId joy == jid' && axis == axis' = Just v
  | otherwise                         = Nothing

-- joyAllButtons :: Joystick -> ([Word8] -> act) -> Input -> IO (Maybe act)
-- joyAllButtons joy mkAct i =
--   return . Just . mkAct $ bs
--   where
--     bs = mapMaybe toButton $ joyButtons i
--     toButton e =
--       let isId = SDL.joyButtonEventWhich e == jsId joy
--           isState = SDL.joyButtonEventState e == SDL.JoyButtonPressed
--       in boolToMaybe (SDL.joyButtonEventButton e) $ isId && isState

-- joyAllAxes :: Joystick -> ([(Word8, Int16)] -> act) -> Input -> IO (Maybe act)
-- joyAllAxes joy mkAct =
--   return . Just . mkAct . mapMaybe toAxis . joyAxes
--   where
--     toAxis e = boolToMaybe (axis, value) isId
--       where
--         axis = SDL.joyAxisEventAxis e
--         value = SDL.joyAxisEventValue e
--         isId = SDL.joyAxisEventWhich e == jsId joy

-- Hat

isHatOn :: SDL.JoyHatPosition -> HatDir -> Bool
isHatOn pos HDUp    = pos `elem` [SDL.HatUp, SDL.HatRightUp, SDL.HatLeftUp]
isHatOn pos HDDown  = pos `elem` [SDL.HatDown, SDL.HatRightDown, SDL.HatLeftDown]
isHatOn pos HDLeft  = pos `elem` [SDL.HatLeft, SDL.HatLeftUp, SDL.HatLeftDown]
isHatOn pos HDRight = pos `elem` [SDL.HatRight, SDL.HatRightUp, SDL.HatRightDown]

joyHat :: HatDir -> InputMotion -> act -> Interpreter act
joyHat hatDir motion act input =
  return $ boolToMaybe (0,act) matchMotion -- TODO: fix PadId
  where
    pPre = isHatOn (preHat input) hatDir
    pCur = isHatOn (curHat input) hatDir
    matchMotion
      | pPre && pCur = Holded == motion
      | pCur         = Pressed == motion
      | pPre         = Released == motion
      | otherwise    = False

-- joyAllHat :: ([HatDir] -> act) -> Input -> IO (Maybe act)
-- joyAllHat mkAct input =
--   return . Just . mkAct $ filter (isHatOn (curHat input)) dirs
--   where
--     dirs = [HDUp, HDRight, HDDown, HDLeft]

-- Haptic

-- | Rumble joystick
-- strength: 0 - 1
-- len:      msec
rumble :: MonadIO m => Joystick -> Double -> Word32 -> m ()
rumble joy strength lengthMSec =
  liftIO $ forM_ (jsHap joy) $ \haptic ->
    HAP.hapticRumblePlay haptic strength' lengthMSec
  where
    strength' = realToFrac strength

-- Utility

boolToMaybe :: a -> Bool -> Maybe a
boolToMaybe a bool
  | bool      = Just a
  | otherwise = Nothing
