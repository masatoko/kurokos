{-# LANGUAGE OverloadedStrings #-}
module Kurokos.UI.Scheme
  (
    StyleMap
  , readStyleMap
  , parseStyleMap
  , makeContextStyle
  ) where

import qualified Control.Exception       as E
import           Control.Lens
import           Data.ByteString         as BS
import           Data.Char               (toLower)
import qualified Data.HashMap.Lazy       as HM
import           Data.List.Extra         (firstJust)
import           Data.List.Split         (chunksOf, splitOn)
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Text               as T
import           Data.Word               (Word8)
import           Data.Yaml               (FromJSON (..), (.:), (.:?))
import qualified Data.Yaml               as Y
import           Linear.V4
import           Safe                    (atMay, readMay)

import           Kurokos.UI.Import       (MonadIO, liftIO)
import           Kurokos.UI.Types
import           Kurokos.UI.Widget       (Widget)
import           Kurokos.UI.Widget.Names (WidgetName, widgetNameOf)

type StyleMap = M.Map String StyleConf

data StyleConf = StyleConf
  { scTextColor   :: Maybe Color
  , scTextAlign   :: Maybe TextAlign
  , scFontIdent   :: Maybe T.Text
  , scFontSize    :: Maybe Int
  , scMargin      :: Maybe (LRTB Int)
  , scTintColor   :: Maybe Color
  , scBgColor     :: Maybe Color
  , scBorderColor :: Maybe Color
  } deriving (Eq, Show)

readStyleMap :: MonadIO m => FilePath -> m StyleMap
readStyleMap path = liftIO $
  either E.throwIO return =<< (parseStyleMap <$> BS.readFile path)

parseStyleMap :: BS.ByteString -> Either Y.ParseException StyleMap
parseStyleMap = Y.decodeEither'

instance FromJSON StyleConf where
  parseJSON (Y.Object v) = StyleConf
    <$> help readColor (v .:? "text-color")
    <*> help readTextAlign (v .:? "text-align")
    <*> v .:? "font"
    <*> v .:? "font-size"
    <*> help readMargin (v .:? "margin")
    <*> help readColor (v .:? "tint-color")
    <*> help readColor (v .:? "background-color")
    <*> help readColor (v .:? "border-color")
    where
      help f = fmap (f =<<)
  parseJSON _ = fail "Expected Object for StyleConf"

readColor :: String -> Maybe Color
readColor str = firstJust id [fromFour, fromHex]
  where
    fromFour = V4 <$> at 0 <*> at 1 <*> at 2 <*> pure (fromMaybe 255 (at 3))
      where
        xs = splitOn " " str
        at i = readMay =<< xs `atMay` i
    fromHex =
      case str of
        '#':str' ->
          let xs = chunksOf 2 str'
              bs = Prelude.map (fromIntegral . readHex) xs
              at i = bs `atMay` i
          in V4 <$> at 0 <*> at 1 <*> at 2 <*> pure (fromMaybe 255 (at 3))
        _ -> Nothing

readHex :: String -> Int
readHex cs =
  let ns = Prelude.map (work . toLower) $ Prelude.reverse cs
      go i n = (16 ^ i) * n
  in sum $ Prelude.zipWith go [0..] ns
  where
    work 'a' = 10
    work 'b' = 11
    work 'c' = 12
    work 'd' = 13
    work 'e' = 14
    work 'f' = 15
    work  n  = read [n]

readTextAlign :: String -> Maybe TextAlign
readTextAlign "left"   = Just TALeft
readTextAlign "right"  = Just TARight
readTextAlign "center" = Just TACenter
readTextAlign _        = Nothing

readMargin :: String -> Maybe (LRTB Int)
readMargin str =
  case mapMaybe readMay $ splitOn " " str of
    (l:r:t:b:_) -> Just $ LRTB l r t b
    _           -> Nothing -- error $ "parse error for margin: " ++ str

-----

makeContextStyle :: Widget -> Maybe WTName -> Maybe WTClass -> StyleMap -> ContextStyle
makeContextStyle widget mName mCls styleMap =
  ContextStyle
    (mkStyle (styleConfsOf normalKeys))
    (mkStyle (styleConfsOf hoverKeys))
  where
    wname = widgetNameOf widget
    mName' = ('$':) <$> mName
    mCls'  = ('.':) <$> mCls

    styleConfsOf :: [String] -> [StyleConf]
    styleConfsOf = mapMaybe (`M.lookup` styleMap)

    normalKeys =
      case (mName', mCls') of
        (Just name', Just cls') -> [wname++name', wname++cls', name', cls', wname, "def"]
        (Just name', Nothing)   -> [wname++name', name', wname, "def"]
        (Nothing, Just cls')    -> [wname++cls', cls', wname, "def"]
        _                       -> [wname, "def"]
    hoverKeys  = Prelude.map (++"@hover") normalKeys ++ normalKeys

    mkStyle :: [StyleConf] -> Style
    mkStyle cs = Style
      { _styleTextColor   = work (V4 0 0 0 255) scTextColor
      , _styleTextAlign   = work TACenter scTextAlign
      , _styleFontIdent   = work "default-font" scFontIdent
      , _styleFontSize    = work 16 scFontSize
      , _styleMargin      = work (LRTB 0 0 0 0) scMargin
      , _styleTintColor   = work (V4 230 230 230 255) scTintColor
      , _styleBgColor     = work (V4 255 255 255 255) scBgColor
      , _styleBorderColor = work (V4 200 200 200 255) scBorderColor
      }
      where
        work def fieldOf = fromMaybe def $ firstJust fieldOf cs
