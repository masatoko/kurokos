{-# LANGUAGE OverloadedStrings #-}
module Kurokos.UI.File.Yaml where

import qualified Data.ByteString.Char8 as BS
import           Data.List             (isPrefixOf)
import           Data.List.Extra       (firstJust)
import qualified Data.Map              as M
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import           Safe                  (readMay)

import           Data.Yaml             (FromJSON (..), (.:), (.:?))
import qualified Data.Yaml             as Y

import qualified Kurokos.Asset         as Asset
import           Kurokos.UI.Core
import           Kurokos.UI.Import
import           Kurokos.UI.Types
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
    , wAsset     :: Maybe Asset.Ident
    , wTitle     :: Maybe Text
    }
  | Container
    { wIdent         :: Maybe String
    , wX             :: UExp
    , wY             :: UExp
    , wWidth         :: UExp
    , wHeight        :: UExp
    , wContainerType :: ContainerType
    , wChildren      :: [YWidget]
    -- Attribute
    , wVisible       :: Maybe Bool
    , wClickable     :: Maybe Bool
    }
  deriving (Eq, Show)

instance FromJSON YWidget where
  parseJSON (Y.Object v) = do
    wtype <- v .: "type"
    case (wtype :: String) of
      "container" -> makeContainer v
      _           -> Single wtype
        <$> v .:? "id"
        <*> getUExp "x" (C 0) v
        <*> getUExp "y" (C 0) v
        <*> getUExp "w" (Rpn "$width") v
        <*> getUExp "h" (Rpn "$height") v
        -- Attribute
        <*> v .:? "visible"
        <*> v .:? "clickable"
        --
        <*> v .:? "asset"
        <*> v .:? "title"
  parseJSON _ = fail "Expected Object for Config value"

makeContainer :: Y.Object -> Y.Parser YWidget
makeContainer v = Container
  <$> v .:? "ident"
  <*> getUExp "x" (C 0) v
  <*> getUExp "y" (C 0) v
  <*> getUExp "w" (Rpn "$width") v
  <*> getUExp "h" (Rpn "$height") v
  <*> (parseContainerType <$> (v .:? "order"))
  <*> v .: "children"
  -- Attribute
  <*> v .:? "visible"
  <*> v .:? "clickable"

getUExp :: Text -> UExp -> Y.Object -> Y.Parser UExp
getUExp label def v = do
  m <- fmap C <$> v .:? label
  mRpn <- fmap Rpn <$> v .:? (label <> "-rpn")
  let uexp = fromMaybe def $ firstJust id [mRpn, m]
  return uexp

parseContainerType :: Maybe String -> ContainerType
parseContainerType Nothing   = Unordered
parseContainerType (Just ct) = work ct
  where
    work "unordered"  = Unordered
    work "horizontal" = HorizontalStack
    work "vertical"   = VerticalStack
    work _            = Unordered -- fail $ "unkown container type: " ++ ct

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
