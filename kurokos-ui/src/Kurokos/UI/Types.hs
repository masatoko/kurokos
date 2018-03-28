{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Kurokos.UI.Types where

import           Control.Lens
import           Data.Default.Class
import           Data.Int           (Int64)
import           Data.Maybe         (fromMaybe)
import           Data.Word          (Word8)
import           Foreign.C.Types    (CInt)
import           Linear.V2
import           Linear.V4

import qualified SDL

import qualified Kurokos.Graphics   as G
import qualified Kurokos.RPN        as RPN

import           Kurokos.UI.Color

type WidgetIdent = Int64
-- | Identity of WidgetTree. It's unique in a GUI.
newtype WTIdent = WTIdent { unWidgetIdent :: WidgetIdent } deriving (Eq, Ord, Show)

-- | Name of WidgetTree defined by a user. It's not always unique.
type WTName = String

-- Size
type GuiPos = SDL.Point V2 CInt
type GuiSize = V2 CInt

data ContainerType
  = Unordered
  | HorizontalStack
  | VerticalStack
  deriving (Eq, Show)

data WidgetState = WidgetState
  { _wstWorldPos :: GuiPos -- ^ Change with setGlobalPos. Must not change directly.
  , _wstPos      :: GuiPos -- ^ Lobal position (Updated on readyRender)
  , _wstWidth    :: Maybe CInt
  , _wstHeight   :: Maybe CInt
  -- , _wstSize     :: GuiSize -- ^ Texture size (Updated on readyRender)
  --
  , _wstVisible  :: Bool
  , _wstHover    :: Bool
  } deriving Show

makeLenses ''WidgetState

iniWidgetState :: WidgetState
iniWidgetState = WidgetState (pure 0) (pure 0) Nothing Nothing True False

wstSize :: WidgetState -> V2 CInt
wstSize wst = V2 w h
  where
    w = fromMaybe (error "Missing width") $ wst^.wstWidth
    h = fromMaybe (error "Missing height") $ wst^.wstHeight

data WidgetAttrib = WidgetAttrib
  { _hoverable :: Bool
  , _clickable :: Bool
  , _draggable :: Bool
  , _droppable :: Bool
  , _visible   :: Bool
  } deriving Show

makeLenses ''WidgetAttrib

defAttrib :: WidgetAttrib
defAttrib = WidgetAttrib
  { _hoverable = True
  , _clickable = True
  , _draggable = False
  , _droppable = False
  , _visible   = True
  }

-- Expression

data Exp
  = ERPN RPN.Exp
  | EConst CInt
  deriving (Eq, Show)

data UExp
  = Rpn String -- RPN expression
  | C Int -- Constant
  deriving (Eq, Show)

fromUExp :: UExp -> Either String Exp
fromUExp (Rpn expr) = ERPN <$> RPN.parse expr
fromUExp (C v)      = return $ EConst $ fromIntegral v

fromUExpV2 :: V2 UExp -> Either String (V2 Exp)
fromUExpV2 (V2 x y) = V2 <$> fromUExp x <*> fromUExp y

kKeyWidth, kKeyHeight, kKeyWinWidth, kKeyWinHeight, kKeyMinWidth, kKeyMinHeight :: String
kKeyWidth     = "width"
kKeyHeight    = "height"
kKeyWinWidth  = "win-width" -- ^ Window width
kKeyWinHeight = "win-height" -- ^ Window height
kKeyMinWidth  = "min-width" -- ^ Minimum required width for parent container
kKeyMinHeight = "min-height" -- ^ Minimum required height for parent container

data CommonResource = CmnRsc
  { cmnrscRectFill   :: G.Prim
  , cmnrscRectBorder :: G.Prim
  , cmnrscTextTex    :: Maybe G.Texture
  } deriving Show

data TextAlign
  = TALeft
  | TARight
  | TACenter
  deriving (Eq, Show)

data LRTB a = LRTB
  { left   :: a
  , right  :: a
  , top    :: a
  , bottom :: a
  } deriving (Eq, Show)

data Style = Style
  { _styleTextAlign :: TextAlign
  , _styleMargin    :: LRTB Int
  } deriving (Eq, Show)

makeLenses ''Style

data WContext = WContext
  { _ctxIdent         :: WTIdent
  , _ctxName          :: Maybe WTName
  , _ctxContainerType :: Maybe ContainerType
  , _ctxAttrib        :: WidgetAttrib
  , _ctxNeedsRender   :: Bool
  , _ctxWidgetState   :: WidgetState
  , _ctxCmnRsc        :: CommonResource
  , _ctxContextColor  :: ContextColor
  , _ctxStyle         :: Style
  , _ctxUPos          :: V2 Exp
  , _ctxUSize         :: V2 Exp
  } deriving Show

makeLenses ''WContext

data WidgetConfig = WidgetConfig
  { wconfName   :: Maybe WTName
  , wconfColor  :: Maybe ContextColor
  , wconfStyle  :: Style
  , wconfAttrib :: Maybe WidgetAttrib
  , wconfPosX   :: UExp
  , wconfPosY   :: UExp
  , wconfWidth  :: UExp
  , wconfHeight :: UExp
  } deriving Show

instance Default WidgetConfig where
  def = WidgetConfig Nothing Nothing style Nothing x y w h
    where
      style = Style TACenter (LRTB 0 0 0 0)
      x = C 0
      y = C 0
      w = Rpn "$min-width"
      h = Rpn "$min-height"

optimumColor :: WContext -> WidgetColor
optimumColor WContext{..}
  | _wstHover = ctxcolHover
  -- | _wstClick = ctxcolClick -- TODO: Implement color when clicked.
  | otherwise = ctxcolNormal
  where
    ContextColor{..} = _ctxContextColor
    WidgetState{..} = _ctxWidgetState

data Cursor = Cursor
  { _cursorPos  :: GuiPos
  , _cursorArea :: SDL.Rectangle CInt
  } deriving (Eq, Show)

makeLenses ''Cursor
