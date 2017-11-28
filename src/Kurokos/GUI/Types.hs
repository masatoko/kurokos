module Kurokos.GUI.Types where

import           Data.Word       (Word8)
import           Foreign.C.Types (CInt)
import           Linear.V2
import           Linear.V4

import qualified SDL

import qualified Kurokos.RPN     as RPN

data Direction
  = Horizontal
  | Vertical
  deriving Show

type GuiPos = V2 CInt
type GuiSize = V2 CInt

type Color = V4 Word8

data WidgetColor = WidgetColor
  { wcBack :: Color
  , wcTint :: Color
  }

data Exp
  = ERPN RPN.Exp
  | EConst CInt

data UExp
  = UERPN String -- RPN expression
  | UEConst Int

fromUExp :: UExp -> Either String Exp
fromUExp (UERPN expr) = ERPN <$> RPN.parse expr
fromUExp (UEConst v)  = return $ EConst $ fromIntegral v

fromUExpV2 :: V2 UExp -> Either String (V2 Exp)
fromUExpV2 (V2 x y) = V2 <$> fromUExp x <*> fromUExp y

keyWidth, keyHeight, keyWinWidth, keyWinHeight :: String
keyWidth     = "w"
keyHeight    = "h"
keyWinWidth  = "winw"
keyWinHeight = "winh"
