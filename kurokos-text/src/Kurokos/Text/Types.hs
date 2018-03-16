module Kurokos.Text.Types where

import qualified Control.Exception.Safe as E
import qualified Data.Map               as M
import qualified Data.Text              as T

type Lang = T.Text

type TextKey = T.Text
type VarKey = T.Text
type VarMap a = M.Map VarKey a

data TextChunk
  = Var VarKey -- ^ Variable text
  | Chunk T.Text -- ^ Constant text
  deriving (Eq, Show)

type LocText = [TextChunk] -- ^ It will be converted to a Text

type TextSet = M.Map TextKey LocText

getText :: TextKey -> VarMap T.Text -> TextSet -> Either String T.Text
getText tkey varmap tset = T.concat <$> toListWith id tkey varmap tset

toListWith :: (T.Text -> a) -> TextKey -> VarMap a -> TextSet -> Either String [a]
toListWith f tkey varmap tset =
  conv =<< M.lookup tkey tset <?> ("Missing TextKey '" ++ T.unpack tkey ++ "' in TextSet. " ++ show (M.keys tset))
  where
    conv = mapM work
      where
        work (Var vkey)   = M.lookup vkey varmap <?> ("Missing VarKey '" ++ T.unpack vkey)
        work (Chunk text) = Right $ f text

toListWithM :: E.MonadThrow m => (T.Text -> m a) -> TextKey -> VarMap a -> TextSet -> m [a]
toListWithM f tkey varmap tset =
  case M.lookup tkey tset of
    Nothing -> E.throw $ userError $ "Missing TextKey '" ++ T.unpack tkey ++ "' in TextSet. " ++ show (M.keys tset)
    Just cs -> conv cs
  where
    conv = mapM work
      where
        work (Var vkey) =
          case M.lookup vkey varmap of
            Nothing -> E.throw $ userError $ "Missing VarKey '" ++ T.unpack vkey
            Just a  -> return a
        work (Chunk text) = f text

(<?>) :: Maybe a -> String -> Either String a
(<?>) Nothing msg = Left msg
(<?>) (Just a) _  = Right a
