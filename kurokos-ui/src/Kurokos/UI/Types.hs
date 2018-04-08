{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Kurokos.UI.Types where

import           Control.Lens
import           Data.Default.Class
import           Data.Int              (Int64)
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import           Data.Word             (Word8)
import qualified Data.Yaml             as Y
import           Foreign.C.Types       (CInt)
import           Linear.V2
import           Linear.V4

import qualified SDL

import qualified Kurokos.Graphics      as G
import qualified Kurokos.RPN           as RPN

import           Kurokos.UI.WidgetTree (WidgetTreePath)

type WidgetIdent = Int64
-- | Identity of WidgetTree. It's unique in a GUI.
newtype WTIdent = WTIdent { unWidgetIdent :: WidgetIdent } deriving (Eq, Ord, Show)

-- | Name of WidgetTree defined by a user. It's not always unique.
type WTName = String

type WTClass = String

-- Size
type GuiPos = SDL.Point V2 CInt
type GuiSize = V2 CInt

type Color = V4 Word8

data ContainerType
  = Unordered
  | HorizontalStack
  | VerticalStack
  deriving (Eq, Show)

data WidgetState = WidgetState
  { _wstWorldPos :: GuiPos -- ^ World position with left-top margin. Change this by setGlobalPos. Do not change directly.
  , _wstLocalPos :: GuiPos -- ^ Lobal position (Updated on readyRender)
  , _wstWidth    :: Maybe CInt
  , _wstHeight   :: Maybe CInt
  , _wstMinSize  :: V2 (Maybe CInt) -- ^ Minimun size to use container
  , _wstShift    :: V2 CInt -- ^ Shift position of children in this container
  , _wstFocus    :: Bool
  --
  , _wstVisible  :: Bool
  , _wstHover    :: Bool
  } deriving Show

makeLenses ''WidgetState

iniWidgetState :: WidgetState
iniWidgetState = WidgetState (pure 0) (pure 0) Nothing Nothing (pure Nothing) (pure 0) False True False

wstSize :: WidgetState -> V2 CInt
wstSize wst = V2 w h
  where
    w = fromMaybe (error "Missing width. Call readyRender before rendering.") $ wst^.wstWidth
    h = fromMaybe (error "Missing height. Call readyRender before rendering.") $ wst^.wstHeight

data WidgetAttrib = WidgetAttrib
  { _hoverable  :: Bool
  , _clickable  :: Bool
  , _draggable  :: Bool
  , _droppable  :: Bool
  , _visible    :: Bool
  , _scrollable :: Bool
  } deriving Show

makeLenses ''WidgetAttrib

defAttrib :: WidgetAttrib
defAttrib = WidgetAttrib
  { _hoverable = True
  , _clickable = True
  , _draggable = False
  , _droppable = False
  , _visible   = True
  , _scrollable = False
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
  { _styleTextColor   :: Color
  , _styleTextAlign   :: TextAlign
  , _styleFontIdent   :: T.Text
  , _styleFontSize    :: Int
  , _styleMargin      :: LRTB Int
  , _styleTintColor   :: Color
  , _styleBgColor     :: Color
  , _styleBorderColor :: Color
  } deriving (Eq, Show)

makeLenses ''Style

data ContextStyle = ContextStyle
  { ctxstNormal :: Style
  , ctxstHover  :: Style
  -- , ctxstClick  :: Style
  } deriving (Eq, Show)

type StyleKey = String

data StyleState = SSNormal | SSHover
  deriving (Eq, Show)

type StyleMap = M.Map StyleKey StyleConf

-- | User settings of Style
data StyleConf = StyleConf
  { scTextColor   :: Maybe Color
  , scTextAlign   :: Maybe TextAlign
  , scFontIdent   :: Maybe T.Text
  , scFontSize    :: Maybe Int
  , scMargin      :: Maybe (LRTB Int)
  , scTintColor   :: Maybe Color
  , scBgColor     :: Maybe Color
  , scBorderColor :: Maybe Color
  } deriving (Eq, Show)

data WContext = WContext
  { _ctxIdent         :: WTIdent
  , _ctxName          :: Maybe WTName
  , _ctxClass         :: Maybe WTClass
  , _ctxPath          :: WidgetTreePath
  , _ctxContainerType :: Maybe ContainerType
  , _ctxAttrib        :: WidgetAttrib
  , _ctxNeedsRender   :: Bool
  , _ctxNeedsResize   :: Bool
  , _ctxWidgetState   :: WidgetState
  , _ctxCmnRsc        :: CommonResource
  , _ctxContextStyle  :: ContextStyle
  , _ctxUPos          :: V2 Exp
  , _ctxUSize         :: V2 Exp
  } deriving Show

makeLenses ''WContext

data WidgetConfig = WidgetConfig
  { wconfName     :: Maybe WTName
  , wconfClass    :: Maybe WTClass
  , wconfStyleMap :: Maybe StyleMap
  , wconfAttrib   :: Maybe WidgetAttrib
  , wconfPosX     :: UExp
  , wconfPosY     :: UExp
  , wconfWidth    :: UExp
  , wconfHeight   :: UExp
  } deriving Show

instance Default WidgetConfig where
  def = WidgetConfig Nothing Nothing Nothing Nothing (C 0) (C 0) (Rpn "$min-width") (Rpn "$min-height")

optimumStyle :: WContext -> Style
optimumStyle ctx
  | wst^.wstHover = ctxstHover
  -- | wst^.wstClick = ctxstClick
  | otherwise     = ctxstNormal
  where
    ContextStyle{..} = ctx^.ctxContextStyle
    wst = ctx^.ctxWidgetState

data Cursor = Cursor
  { _cursorPos  :: GuiPos
  , _cursorArea :: SDL.Rectangle CInt
  } deriving (Eq, Show)

makeLenses ''Cursor
