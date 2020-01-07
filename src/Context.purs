module QueryStringPSQL.Context where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Prelude (class Show, (<>))


type StrMap a = Map.Map String a

data QueryEngine = PostgreSql | Redshift

derive instance genericQueryEngine :: Generic QueryEngine _

instance showQueryEngine :: Show QueryEngine where
  show = genericShow

instance encodeJsonQueryEngine :: EncodeJson QueryEngine where
  encodeJson = genericEncodeJson

instance decodeJsonQueryEngine :: DecodeJson QueryEngine where
  decodeJson = genericDecodeJson

----

data LMapType = Simple String | CastNulls String | Expr String

instance showLMapType :: Show LMapType where
  show (Simple s) = "Simple " <> s
  show (CastNulls s) = "CastNulls " <> s
  show (Expr s) = "Expr " <> s

derive instance genericLMapType :: Generic LMapType _

instance encodeJsonLMapType :: EncodeJson LMapType where
  encodeJson = genericEncodeJson

instance decodeJsonLMapType :: DecodeJson LMapType where
  decodeJson = genericDecodeJson

----

data TimezoneInfo = WithTimezone | WithoutTimezone Number

derive instance genericTimezoneInfo :: Generic TimezoneInfo _

instance showTimezoneInfo :: Show TimezoneInfo where
  show = genericShow

instance encodeJsonTimezoneInfo :: EncodeJson TimezoneInfo where
  encodeJson = genericEncodeJson

instance decodeJsonTimezoneInfo :: DecodeJson TimezoneInfo where
  decodeJson = genericDecodeJson

---


type QueryContext = {
    tableAlias :: String,
    timestampColumn :: {
      name :: String,
      timezone :: TimezoneInfo
    },
    fieldMap :: StrMap LMapType,
    engine :: QueryEngine
  }
-- derive instance genericQueryContext :: Generic QueryContext _
-- instance showQueryContext :: Show QueryContext where
--   show = genericShow
