{-# LANGUAGE OverloadedStrings #-}
module Kurokos.GUI.File.Yaml where

import qualified Data.ByteString.Char8 as BS
import           Data.List             (isPrefixOf)
import           Data.List.Extra       (firstJust)
import qualified Data.Map              as M
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import           Safe                  (readMay)

import           Data.Yaml             (FromJSON (..), (.:), (.:?))
import qualified Data.Yaml             as Y

import           Kurokos.GUI.Core
import           Kurokos.GUI.Import
import           Kurokos.GUI.Types
import qualified Kurokos.RPN           as RPN

decodeWidgets :: BS.ByteString -> Either Y.ParseException YWidgets
decodeWidgets = Y.decodeEither'

data GuiConf = GuiConf
  { defaultFontPath :: FilePath
  } deriving (Eq, Show)

type YWidgets = [YWidget]

data YWidget
  = Single
    { wType      :: String
    , wIdent     :: Maybe String
    , wX         :: UExp
    , wY         :: UExp
    , wWidth     :: UExp
    , wHeight    :: UExp
    -- Attribute
    , wVisible   :: Maybe Bool
    , wClickable :: Maybe Bool
    --
    , wPath      :: Maybe FilePath
    , wTitle     :: Maybe Text
    } deriving (Eq, Show)
  -- | Container
  --   { wIdent :: String
  --   }

instance FromJSON YWidget where
  parseJSON (Y.Object v) = do
    wtype <- v .: "type"
    case (wtype :: String) of
      "container" -> makeContainer v
      _           -> Single wtype
        <$> v .:? "ident"
        <*> getUExp "x" (C 0) v
        <*> getUExp "y" (C 0) v
        <*> getUExp "w" (Rpn "$width") v
        <*> getUExp "h" (Rpn "$height") v
        -- Attribute
        <*> v .:? "visible"
        <*> v .:? "clickable"
        --
        <*> v .:? "path"
        <*> v .:? "title"
  parseJSON _ = fail "Expected Object for Config value"

makeContainer v = undefined

getUExp :: Text -> UExp -> Y.Object -> Y.Parser UExp
getUExp label def v = do
  m <- fmap C <$> v .:? label
  mRpn <- fmap Rpn <$> v .:? (label <> "-rpn")
  let uexp = fromMaybe def $ firstJust id [mRpn, m]
  return uexp

-- validateUExp :: UExp -> Either String Double
-- validateUExp uexp =
--   case fromUExp uexp of
--     Left err -> Left err
--     Right e  ->
--       case evalExp e of
--         Left err -> Left err
--         Right e' -> e' `deepseq` Right e'
--   where
--     vmap = M.fromList [(k,0) | k <- keys]
--     keys = ["width", "height", "x", "y"]
--
--     evalExp (ERPN expr) = RPN.eval vmap expr
--     evalExp (EConst v)  = return $ fromIntegral v
