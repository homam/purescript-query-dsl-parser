module QueryStringPSQL.Params where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Prelude (class Eq, class Ord, class Show)

-- SqlCol is either a simple string or a the column and JSON path
-- Currently we support simple JSON string hashes
data SqlCol = SqlColNormal String | SqlColJSON {colName :: String, jsonField :: String}
derive instance genericSqlCol :: Generic SqlCol _
instance showSqlCol :: Show SqlCol where
  show = genericShow
instance eqSqlCol :: Eq SqlCol where
  eq = genericEq
instance ordSqlCol :: Ord SqlCol where
  compare a b = genericCompare a b
instance encodeJsonSqlCol :: EncodeJson SqlCol where
  encodeJson = genericEncodeJson
instance decodeJsonSqlCol :: DecodeJson SqlCol where
  decodeJson = genericDecodeJson



data SortOrder = ASC | DESC
derive instance genericSortOrder :: Generic SortOrder _
instance showSortOrder :: Show SortOrder where
  show = genericShow

-- Sort by and order
data Sort = Sort {
  by :: SqlCol,
  order :: SortOrder
}
derive instance genericSort :: Generic Sort _
instance showSort :: Show Sort where
  show = genericShow


data BreakdownDetails = BreakdownDetails {
  sort :: Maybe Sort,
  valuesFilter :: Maybe Filters
}
derive instance genericBreakdownDetails :: Generic BreakdownDetails _

instance showBreakdownDetails :: Show BreakdownDetails where
  show = genericShow

emptyBreakdownDetails :: BreakdownDetails
emptyBreakdownDetails = BreakdownDetails {
    sort: Nothing,
    valuesFilter: Nothing
  }

---

type SingleBreakdown = Tuple SqlCol BreakdownDetails

---

type Breakdown = List.List SingleBreakdown

emptyBreakdown :: Breakdown
emptyBreakdown = List.Nil

---

data LikePosition = LikeBefore | LikeAfter | LikeBoth
derive instance genericLikePosition :: Generic LikePosition _
instance showLikePosition :: Show LikePosition where
  show = genericShow
instance eqLikePosition :: Eq LikePosition where
  eq = genericEq
instance encodeJsonLikePosition :: EncodeJson LikePosition where
  encodeJson = genericEncodeJson
instance decodeJsonLikePosition :: DecodeJson LikePosition where
  decodeJson = genericDecodeJson


data FilterVal = 
    FilterValStr String 
  | FilterValUnquotedInt Int 
  | FilterValUnquotedNumber Number
derive instance genericFilterVal :: Generic FilterVal _
instance showFilterVal :: Show FilterVal where
  show = genericShow
instance eqFilterVal :: Eq FilterVal where
  eq = genericEq
instance encodeJsonFilterVal :: EncodeJson FilterVal where
  encodeJson = genericEncodeJson
instance decodeJsonFilterVal :: DecodeJson FilterVal where
  decodeJson = genericDecodeJson

data FilterLang = 
    FilterIn (List.List FilterVal) 
  | FilterEq FilterVal
  | FilterRange FilterVal FilterVal
  | FilterLike LikePosition String 
  | FilterNot FilterLang

derive instance genericFilterLang :: Generic FilterLang _
instance showFilterLang :: Show FilterLang where
  show a = genericShow a
instance eqFilterLang :: Eq FilterLang where
  eq a b = genericEq a b
instance encodeJsonFilterLang :: EncodeJson FilterLang where
  encodeJson x = genericEncodeJson x
instance decodeJsonFilterLang :: DecodeJson FilterLang where
  decodeJson x = genericDecodeJson x


type Filters = Map.Map SqlCol FilterLang
emptyFilters :: Filters
emptyFilters = Map.empty


type QueryParams = {
  timezone :: Number,
  dateFrom :: String,
  dateTo :: String,
  breakdown :: Breakdown,
  filters :: Filters
}

emptyQueryParams :: QueryParams
emptyQueryParams = {timezone: toNumber 0, dateFrom: "2019-11-22", dateTo: "2019-11-30", breakdown: emptyBreakdown, filters: emptyFilters  } :: QueryParams