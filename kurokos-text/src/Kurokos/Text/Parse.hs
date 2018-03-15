module Kurokos.Text.Parse
  ( parseTextSet
  ) where

import           Control.Monad.Combinators
import qualified Data.ByteString           as BS
import qualified Data.HashMap.Strict       as HM
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import           Data.Void
import           Data.Yaml                 ((.:))
import qualified Data.Yaml                 as Y
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Kurokos.Text.Types

type Parser = Parsec Void String

parseLocText :: String -> Maybe LocText
parseLocText = parseMaybe loctext

-- | LocText (= [TextChunk]) parser
loctext :: Parser LocText
loctext = many (var <|> chunk)

-- | Var parser
var :: Parser TextChunk
var = do
  string "${"
  key <- some (noneOf "}")
  string "}"
  return $ Var $ T.pack key

-- | Chunk parser
chunk :: Parser TextChunk
chunk = do
  content <- some (noneOf "${")
  return . Chunk $ T.pack content

--

parseTextSet :: BS.ByteString -> Lang -> Either String TextSet
parseTextSet bytes lang =
  case Y.decodeEither bytes of
    Left errmsg -> Left errmsg
    Right obj   -> Y.parseEither (textSetParser lang) obj

textSetParser :: Lang -> Y.Value -> Y.Parser TextSet
textSetParser lang (Y.Object root) =
  fmap M.fromList $ mapM mkPair $ HM.keys root
  where
    mkPair :: TextKey -> Y.Parser (TextKey, LocText)
    mkPair key =
      case HM.lookup key root of
        Just obj -> (,) key <$> go obj
        Nothing  -> fail "Missing key"
      where
        go (Y.Object v) = do
          str <- T.unpack <$> v .: lang
          case parseLocText str of
            Nothing -> fail $ "Parse error: " ++ str
            Just a  -> return a
        go _ = fail "Expected Object for TextSet (child)"
textSetParser _ _ = fail "Expected Object for TextSet (parent)"
