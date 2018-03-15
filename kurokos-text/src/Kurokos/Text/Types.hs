module Kurokos.Text.Types where

import qualified Data.Map  as M
import qualified Data.Text as T

type Lang = T.Text

type TextKey = T.Text
type VarKey = T.Text
type VarMap = M.Map VarKey T.Text

data TextChunk
  = Var VarKey -- ^ Variable text
  | Chunk T.Text -- ^ Constant text
  deriving (Eq, Show)

type LocText = [TextChunk] -- ^ It will be converted to a Text

type TextSet = M.Map TextKey LocText

getText :: TextKey -> VarMap -> TextSet -> Either String T.Text
getText tkey varmap tset =
  toText =<< M.lookup tkey tset <?> ("Missing TextKey '" ++ T.unpack tkey ++ "' in TextSet. " ++ show (M.keys tset))
  where
    toText :: [TextChunk] -> Either String T.Text
    toText cs = T.concat <$> mapM work cs
      where
        work :: TextChunk -> Either String T.Text
        work (Var vkey)   = M.lookup vkey varmap <?> ("Missing VarKey '" ++ T.unpack vkey ++ "' in " ++ show varmap)
        work (Chunk text) = Right text

(<?>) :: Maybe a -> String -> Either String a
(<?>) Nothing msg = Left msg
(<?>) (Just a) _  = Right a
