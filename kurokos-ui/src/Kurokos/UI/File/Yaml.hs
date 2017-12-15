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

import SDL.Font as Font

import qualified Kurokos.Asset         as Asset
import qualified Kurokos.RPN           as RPN
import           Kurokos.UI.Color      (ContextColor)
import           Kurokos.UI.Core
import           Kurokos.UI.Import
import           Kurokos.UI.Types

decodeWidgets :: BS.ByteString -> Either Y.ParseException YWidgets
decodeWidgets = Y.decodeEither'

type YWidgets = [YWidget]

data YWidget
  = Single
    { wType      :: String
    , wIdent     :: Maybe String
    , wColor     :: Maybe ContextColor
    , wX         :: UExp
    , wY         :: UExp
    , wWidth     :: UExp
    , wHeight    :: UExp
    -- Attribute
    , wVisible   :: Maybe Bool
    , wClickable :: Maybe Bool
    , wHoverable :: Maybe Bool
    --
    , wAsset     :: Maybe Asset.Ident
    --
    , wTitle     :: Maybe Title
    }
  | Container
    { wIdent         :: Maybe String
    , wColor         :: Maybe ContextColor
    , wX             :: UExp
    , wY             :: UExp
    , wWidth         :: UExp
    , wHeight        :: UExp
    , wContainerType :: ContainerType
    , wChildren      :: [YWidget]
    -- Attribute
    , wVisible       :: Maybe Bool
    , wClickable     :: Maybe Bool
    , wHoverable     :: Maybe Bool
    }
  deriving (Eq, Show)

instance FromJSON YWidget where
  parseJSON (Y.Object v) = do
    wtype <- v .: "type"
    case (wtype :: String) of
      "container" -> makeContainer v
      _           -> Single wtype
        <$> v .:? "id"
        <*> v .:? "color"
        <*> getUExp "x" (C 0) v
        <*> getUExp "y" (C 0) v
        <*> getUExp "w" (Rpn "$width") v
        <*> getUExp "h" (Rpn "$height") v
        -- Attribute
        <*> v .:? "visible"
        <*> v .:? "clickable"
        <*> v .:? "hoverable"
        --
        <*> v .:? "asset"
        <*> v .:? "title"
  parseJSON _ = fail "Expected Object for Config value"

makeContainer :: Y.Object -> Y.Parser YWidget
makeContainer v = Container
  <$> v .:? "id"
  <*> v .:? "color"
  <*> getUExp "x" (C 0) v
  <*> getUExp "y" (C 0) v
  <*> getUExp "w" (Rpn "$width") v
  <*> getUExp "h" (Rpn "$height") v
  <*> (parseContainerType <$> (v .:? "order"))
  <*> v .: "children"
  -- Attribute
  <*> v .:? "visible"
  <*> v .:? "clickable"
  <*> v .:? "hoverable"

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


data Title
  = Title Text Font.PointSize Asset.Ident
  deriving (Eq, Show)

instance FromJSON Title where
  parseJSON (Y.Object v) = Title
    <$> v .: "text"
    <*> v .: "size"
    <*> v .: "asset"
  parseJSON _ = fail "Expected Object for Title"
