{-# LANGUAGE OverloadedStrings #-}
module Kurokos.UI.File.Yaml where

import qualified Data.ByteString.Char8 as BS
import           Data.List             (isPrefixOf)
import           Data.List.Extra       (firstJust)
import           Data.List.Split       (splitOn)
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe, mapMaybe)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Safe                  (readMay)

import           Data.Yaml             (FromJSON (..), (.:), (.:?))
import qualified Data.Yaml             as Y

import qualified Kurokos.Asset.Raw     as Asset
import           Kurokos.Graphics      (FontSize)
import qualified Kurokos.RPN           as RPN
import           Kurokos.UI.Core
import           Kurokos.UI.Import
import           Kurokos.UI.Scheme
import           Kurokos.UI.Types

decodeWidgets :: BS.ByteString -> Either Y.ParseException YWidgets
decodeWidgets = Y.decodeEither'

type YWidgets = [YWidget]

data Area_ = Area_
  { areaX :: Int
  , areaY :: Int
  , areaW :: Int
  , areaH :: Int
  } deriving (Eq, Show)

instance FromJSON Area_ where
  parseJSON (Y.Object v) = Area_
    <$> v .: "x"
    <*> v .: "y"
    <*> v .: "w"
    <*> v .: "h"

-- | Value for slider
data YValue
  = YValue
    { yvType :: String
    , yvDef  :: Maybe Double
    , yvMin  :: Double
    , yvMax  :: Double
    }
  deriving (Eq, Show)

instance FromJSON YValue where
  parseJSON (Y.Object v) = YValue
    <$> v .: "type"
    <*> v .:? "def"
    <*> v .: "min"
    <*> v .: "max"

data YPicker = YPicker
  { yElems  :: [T.Text]
  , yKeys   :: [String]
  , yDefKey :: Maybe String
  } deriving (Eq, Show)

instance FromJSON YPicker where
  parseJSON (Y.Object v) = YPicker
    <$> v .: "elems"
    <*> v .: "keys"
    <*> v .:? "def"

data YWidget
  = Single
    { wType     :: String
    , wName     :: Maybe String
    , wClass    :: Maybe String
    , wX        :: UExp
    , wY        :: UExp
    , wWidth    :: UExp
    , wHeight   :: UExp
    , wArea     :: Maybe Area_
    , wAttrib   :: Maybe YWidgetAttrib
    , wValue    :: Maybe YValue
    , wPicker   :: Maybe YPicker
    --
    , wAsset    :: Maybe Asset.Ident
    --
    , wText     :: Maybe T.Text
    , wStyleMap :: Maybe StyleMap
    }
  | Container
    { wName          :: Maybe String
    , wClass         :: Maybe String
    , wX             :: UExp
    , wY             :: UExp
    , wWidth         :: UExp
    , wHeight        :: UExp
    , wContainerType :: ContainerType
    , wChildren      :: [YWidget]
    , wAttrib        :: Maybe YWidgetAttrib
    , wStyleMap      :: Maybe StyleMap
    }
  deriving (Eq, Show)

instance FromJSON YWidget where
  parseJSON (Y.Object v) = do
    wtype <- v .: "type"
    case (wtype :: String) of
      "container" -> makeContainer v
      _           -> Single wtype
        <$> v .:? "name"
        <*> v .:? "class"
        <*> getUExp "x" (C 0) v
        <*> getUExp "y" (C 0) v
        <*> getUExp "w" (Rpn "$min-width") v
        <*> getUExp "h" (Rpn "$min-height") v
        <*> v .:? "area"
        <*> v .:? "attrib"
        <*> v .:? "value" -- Value
        <*> v .:? "picker"
        --
        <*> v .:? "asset"
        <*> v .:? "text"
        <*> v .:? "style"
  parseJSON _ = fail "Expected Object for Config value"

makeContainer :: Y.Object -> Y.Parser YWidget
makeContainer v = Container
  <$> v .:? "name"
  <*> v .:? "class"
  <*> getUExp "x" (C 0) v
  <*> getUExp "y" (C 0) v
  <*> getUExp "w" (Rpn "$min-width") v
  <*> getUExp "h" (Rpn "$min-height") v
  <*> (parseContainerType <$> (v .:? "order"))
  <*> (fromMaybe [] <$> v .:? "children")
  <*> v .:? "attrib"
  <*> v .:? "style"

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

data YWidgetAttrib = YWidgetAttrib
  { ywaHoverable  :: Maybe Bool
  , ywaClickable  :: Maybe Bool
  , ywaDraggable  :: Maybe Bool
  , ywaDroppable  :: Maybe Bool
  , ywaVisible    :: Maybe Bool
  , ywaScrollable :: Maybe Bool
  } deriving (Eq, Show, Read)

instance FromJSON YWidgetAttrib where
  parseJSON (Y.Object v) = YWidgetAttrib
    <$> v .:? "hoverable"
    <*> v .:? "clickable"
    <*> v .:? "draggable"
    <*> v .:? "droppable"
    <*> v .:? "visible"
    <*> v .:? "scrollable"
  parseJSON _ = fail "Expected Object for YWidgetAttrib"

-- instance FromJSON Style where
--   parseJSON (Y.Object v) = Style
--     <$> (maybe TACenter parseTextAlign <$> v .:? "text-align")
--     <*> (maybe defMargin parseMargin <$> v .:? "margin")

-- parseTextAlign :: String -> TextAlign
-- parseTextAlign "left"   = TALeft
-- parseTextAlign "right"  = TARight
-- parseTextAlign "center" = TACenter

-- parseMargin :: String -> LRTB Int
-- parseMargin str =
--   case mapMaybe readMay $ splitOn " " str of
--     (l:r:t:b:_) -> LRTB l r t b
--     _           -> error $ "parse error for margin: " ++ str

-- defStyle :: Style
-- defStyle = Style textAlign defMargin
--   where
--     textAlign = TACenter

-- defMargin :: LRTB Int
-- defMargin = LRTB 4 4 4 4
