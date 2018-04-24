module Kurokos.Text.Types where

import           Data.Either       (isLeft)
import           Data.Either.Extra (fromRight)
import           Data.List         (find)
import qualified Data.Map          as M
import qualified Data.Text         as T

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

member :: TextKey -> TextSet -> Bool
member = M.member

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

toListWithM :: Monad m => (T.Text -> m a) -> TextKey -> VarMap a -> TextSet -> m (Either String [a])
toListWithM f tkey varmap tset =
  case M.lookup tkey tset of
    Nothing -> return . Left $ "Missing TextKey '" ++ T.unpack tkey ++ "' in TextSet. " ++ show (M.keys tset)
    Just cs -> conv cs
  where
    conv cs = do
      cs' <- mapM work cs
      case find isLeft cs' of
        Just (Left errmsg) -> return $ Left errmsg
        _                  -> return $ Right $ map fromRight cs'
      where
        work (Var vkey) =
          case M.lookup vkey varmap of
            Nothing -> return . Left $ "Missing VarKey '" ++ T.unpack vkey
            Just a  -> return $ Right a
        work (Chunk text) = Right <$> f text

(<?>) :: Maybe a -> String -> Either String a
(<?>) Nothing msg = Left msg
(<?>) (Just a) _  = Right a
