module Kurokos.GUI.Types where

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

data Direction
  = Horizontal
  | Vertical
  deriving Show

type GuiPos = SDL.Point V2 CInt
type GuiSize = V2 CInt

type Color = V4 Word8

data WidgetColor = WidgetColor
  { wcBack :: Color
  , wcTint :: Color
  , wcFont :: Color
  } deriving Show

data Exp
  = ERPN RPN.Exp
  | EConst CInt
  deriving Show

data UExp
  = Rpn String -- RPN expression
  | C Int -- Constant
  deriving Show

fromUExp :: UExp -> Either String Exp
fromUExp (Rpn expr) = ERPN <$> RPN.parse expr
fromUExp (C v)  = return $ EConst $ fromIntegral v

fromUExpV2 :: V2 UExp -> Either String (V2 Exp)
fromUExpV2 (V2 x y) = V2 <$> fromUExp x <*> fromUExp y

keyWidth, keyHeight, keyWinWidth, keyWinHeight :: String
keyWidth     = "width"
keyHeight    = "height"
keyWinWidth  = "winwidth"
keyWinHeight = "winheight"
