module Kurokos.GUI.Types where

import           Foreign.C.Types (CInt)
import           Linear.V2

import qualified Kurokos.RPN as RPN

data Direction
  = Horizontal
  | Vertical
  deriving Show

type GuiPos = V2 CInt
type GuiSize = V2 CInt

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
