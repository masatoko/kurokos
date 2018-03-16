{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Kurokos.UI.Types where

import           Control.Lens
import           Data.Int        (Int64)
import           Data.Word       (Word8)
import           Foreign.C.Types (CInt)
import           Linear.V2
import           Linear.V4

import qualified SDL

import qualified Kurokos.RPN     as RPN
import qualified Kurokos.Graphics as G

import Kurokos.UI.Color

type WidgetIdent = Int64
-- | Identity of WidgetTree. It's unique in a GUI.
newtype WTIdent = WTIdent WidgetIdent deriving (Eq, Show)

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
  { _wstGlobalPos :: GuiPos -- Change via setGlobalPos. Must not change directly.
  , _wstPos       :: GuiPos -- Lobal position
  , _wstSize      :: GuiSize -- Texture size
  --
  , _wstVisible   :: Bool
  , _wstHover     :: Bool
  }

makeLenses ''WidgetState

iniWidgetState :: WidgetState
iniWidgetState = WidgetState (pure 0) (pure 0) (pure 0) True False

data WidgetAttrib = WidgetAttrib
  { _hoverable :: Bool
  , _clickable :: Bool
  , _visible   :: Bool
  }

makeLenses ''WidgetAttrib

defAttrib :: WidgetAttrib
defAttrib = WidgetAttrib
  { _hoverable = True
  , _clickable = True
  , _visible = True
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

keyWidth, keyHeight, keyWinWidth, keyWinHeight :: String
keyWidth     = "width"
keyHeight    = "height"
keyWinWidth  = "winwidth"
keyWinHeight = "winheight"

data CommonResource = CmnRsc
  { cmnrscRectFill   :: G.Prim
  , cmnrscRectBorder :: G.Prim
  , cmnrscTextTex    :: Maybe G.Texture
  }

data TextAlign
  = TALeft
  | TARight
  | TACenter
  deriving (Eq, Show)

data Style = Style
  { _styleTextAlign :: TextAlign
  } deriving Show

makeLenses ''Style
data WContext = WContext
  { _ctxIdent         :: WTIdent
  , _ctxName          :: Maybe WTName
  , _ctxContainerType :: Maybe ContainerType
  , _ctxAttrib        :: WidgetAttrib
  , _ctxNeedsLayout   :: Bool
  , _ctxNeedsRender   :: Bool
  , _ctxWidgetState   :: WidgetState
  , _ctxCmnRsc        :: CommonResource
  , _ctxContextColor  :: ContextColor
  , _ctxStyle         :: Style
  , _ctxUPos          :: V2 Exp
  , _ctxUSize         :: V2 Exp
  }

makeLenses ''WContext

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
