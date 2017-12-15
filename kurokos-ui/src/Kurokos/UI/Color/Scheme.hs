{-# LANGUAGE OverloadedStrings #-}
module Kurokos.UI.Color.Scheme
  (
    ColorScheme
  , readColorScheme
  , parseColorScheme
  , lookupColorOfWidget
  ) where

import qualified Control.Exception       as E
import           Control.Lens
import           Data.ByteString         as BS
import qualified Data.HashMap.Lazy       as HM
import           Data.List.Extra         (firstJust)
import           Data.List.Split         (splitOn)
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes)
import qualified Data.Text               as T
import           Data.Word               (Word8)
import           Data.Yaml               (FromJSON (..), (.:), (.:?))
import qualified Data.Yaml               as Y

import           SDL.Vect

import           Kurokos.UI.Color        (ContextColor)
import           Kurokos.UI.Import
import           Kurokos.UI.Widget       (Widget)
import           Kurokos.UI.Widget.Names (WidgetName, widgetNameOf)

type ColorScheme = M.Map WidgetName ContextColor

readColorScheme :: FilePath -> IO ColorScheme
readColorScheme path =
  either E.throwIO return =<< (parseColorScheme <$> BS.readFile path)

parseColorScheme :: BS.ByteString -> Either Y.ParseException ColorScheme
parseColorScheme bytes =
  unColorScheme_ <$> Y.decodeEither' bytes

lookupColorOfWidget :: Widget -> ColorScheme -> Either String ContextColor
lookupColorOfWidget w scheme =
  case firstJust (`M.lookup` scheme) [key, def] of
    Nothing  -> Left $ "Missing '" ++ key ++ "' in color scheme. Or set '" ++ def ++ "' context color."
    Just col -> Right col
  where
    key = "_" ++ widgetNameOf w
    def = "_default"

-- Internal

newtype ColorScheme_ = ColorScheme_ { unColorScheme_ :: ColorScheme }

instance FromJSON ColorScheme_ where
  parseJSON (Y.Object v) =
    ColorScheme_ . M.fromList . catMaybes <$> mapM parseOne (HM.keys v)
    where
      parseOne :: T.Text -> Y.Parser (Maybe (WidgetName, ContextColor))
      parseOne wname = fmap ((,) (T.unpack wname)) <$> v .:? wname

  parseJSON _ = fail "Expected Object for ColorScheme"
