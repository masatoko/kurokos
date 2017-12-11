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

type Key = Int64
newtype WTKey = WTKey Key deriving Show

type WidgetIdent = String

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

-- Color

type Color = V4 Word8

data WidgetPart a = WP
  { wpBack :: a
  , wpTint :: a
  , wpFont :: a
  }

instance Functor WidgetPart where
  fmap f WP{..} = WP (f wpBack) (f wpTint) (f wpFont)

newtype WidgetColor = WC { unWC :: WidgetPart Color }
newtype WidgetColorModifier = WCM { unWCM :: WidgetPart (Color -> Color) }
data ColorSet = ColorSet
  { colorSetBasis :: WidgetColor
  , colorSetHover :: WidgetColorModifier
  }

modColor :: WidgetColorModifier -> WidgetColor -> WidgetColor
modColor (WCM m) (WC a) =
  WC $ WP (work wpBack wpBack) (work wpTint wpTint) (work wpFont wpFont)
  where
    work f f' = f m (f' a)

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

data WContext = WContext
  { _ctxKey           :: WTKey
  , _ctxIdent         :: Maybe WidgetIdent
  , _ctxContainerType :: Maybe ContainerType
  , _ctxAttrib        :: WidgetAttrib
  , _ctxNeedsLayout   :: Bool
  , _ctxNeedsRender   :: Bool
  , _ctxWidgetState   :: WidgetState
  , _ctxColorSet      :: ColorSet
  , _ctxColor         :: WidgetColor
  , _ctxTexture       :: SDL.Texture
  , _ctxUPos          :: V2 Exp
  , _ctxUSize         :: V2 Exp
  }

makeLenses ''WContext

data Cursor = Cursor
  { _cursorPos  :: GuiPos
  , _cursorArea :: SDL.Rectangle CInt
  } deriving Show

makeLenses ''Cursor
