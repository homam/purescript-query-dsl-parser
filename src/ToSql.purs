module QueryStringPSQL.ToSql where
  
import Data.Int (toNumber)
import Data.List (fromFoldable, intercalate)
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Math (floor)
import Prelude (map, negate, show, ($), (*), (<), (<<<), (<>), (==), (>))
import QueryStringPSQL.Context (LMapType(..), QueryContext, QueryEngine(..), TimezoneInfo(..))
import QueryStringPSQL.Params (BreakdownDetails, FilterLang(..), FilterVal(..), LikePosition(..), QueryParams, SqlCol(..))

appendSpace :: String -> String -> String
appendSpace a b = a <> " " <> b
infixr 5 appendSpace as <#>

class ToSql a where
  toSql :: QueryParams -> QueryContext -> a -> String

instance toSqlSqlCol :: ToSql SqlCol where
  toSql params ctx c = defaultCast params ctx c
  -- toSql (QueryParams {timezone}) context (SqlColNormal c) = defaultCast timezone context c
  -- toSql (QueryParams {timezone}) context (SqlColJSON {colName, jsonField}) = alias context colName <> "->>" <> "'" <> jsonField <> "'"

instance lMapTypeToSQL :: ToSql LMapType where
  toSql _ context (Simple s) = context.tableAlias <> ".\"" <> s <> "\""
  toSql _ context (CastNulls s) = "coalesce(cast(" <> context.tableAlias <> ".\"" <> s <> "\" as varchar), 'Unknown')" 
  toSql _ context (Expr s) = "(" <> s <> ")"

instance singleBreakdownToSQL :: ToSql (Tuple SqlCol BreakdownDetails) where
  toSql params context (Tuple col _) = 
    let dimName = case col of
          SqlColNormal c -> c
          SqlColJSON j   -> j.colName <> "->" <> j.jsonField 
    in  toSql params context col <> " AS " <> "\"d_" <> dimName <> "\""

instance breakdownToSql :: ToSql (List.List (Tuple SqlCol BreakdownDetails)) where
  toSql params context list = List.intercalate ",\n" (map (toSql params context) list)

instance filtersLangToSql :: ToSql (Tuple SqlCol FilterLang) where
  toSql params context (Tuple col lang) = filterLangToStr' (toSql params context col) lang
    where
      filterLangToStr' :: String -> FilterLang -> String
      filterLangToStr' col' (FilterIn vals) = intercalate " OR " $ map ((\v -> col' <> " = "  <> v) <<< filterValToStr) vals 
      filterLangToStr' col' (FilterEq val) =  col' <> " = " <> filterValToStr val
      filterLangToStr' col' (FilterRange a b) = col' <> " >= "  <> filterValToRangeStr a <> " AND " <> col' <> " < " <> filterValToRangeStr b
      filterLangToStr' col' (FilterNot fl) = "NOT (" <> filterLangToStr' col' fl <> ")"
      filterLangToStr' col' (FilterLike lk s) = col' <> " LIKE " <> inSq (
          case lk of 
            LikeAfter -> s <> "%"
            LikeBefore -> "%" <> s
            LikeBoth -> "%" <> s <> "%"
          )

      filterValToStr :: FilterVal -> String
      filterValToStr (FilterValStr s) = inSq s
      filterValToStr (FilterValUnquotedInt i) = show i
      filterValToStr (FilterValUnquotedNumber i) = show i

      filterValToRangeStr :: FilterVal -> String
      filterValToRangeStr (FilterValUnquotedInt i) = show i
      filterValToRangeStr (FilterValUnquotedNumber i) = show i
      filterValToRangeStr x = " NOT SUPPORTED " <> show x

      inSq :: String -> String
      inSq s = "'" <> s <> "'"

instance filtersToSql :: ToSql (Map.Map SqlCol FilterLang) where
  toSql params context filters = intercalate (newLine <> "AND ") $ fromFoldable rest
    where

      newLine = "\n" -- <> indent

      toAscArray :: forall k v. Map.Map k v -> Array (Tuple k v)
      toAscArray = Map.toUnfoldable

      rest = map (\x -> "(" <> x <> ")") $ map (toSql params context) $ toAscArray $ filters




alias :: QueryContext -> String -> String
alias ctx col = ctx.tableAlias <> "." <> "\"" <> col <> "\""


defaultCast :: QueryParams -> QueryContext -> SqlCol -> String
defaultCast params context = go where
  go (SqlColNormal c) = normal c
  go (SqlColJSON {colName, jsonField}) = alias context colName <> "->>" <> "'" <> jsonField <> "'"
  normal "$hour" = timeDim "hour"
  normal "$day" = timeDim "day"
  normal "$week" = timeDim "week"
  normal "$month" = timeDim "month"
  normal "$minute" = timeDim "minute"
  normal col =
    toSql params context $ fromMaybe (Simple col) (Map.lookup col context.fieldMap)

  alias' = alias context
  timeDim dim = case context.timestampColumn.timezone of
    WithTimezone -> "date_trunc('" <> dim <> "', " <> context.timestampColumn.name <> ")"
    WithoutTimezone dbTimezone -> if dbTimezone == zeroN 
      then "date_trunc('" <> dim <> "', " <> context.timestampColumn.name <> ")"
      else "date_trunc('" <> dim <> "', (" <> context.timestampColumn.name <> " - '" <> tz dbTimezone <> " hour' :: INTERVAL))"

  tz dbTimezone = show $ floor $ negativeOneN * dbTimezone --TODO: floor is a hack for redshift



timestampFiltersToSqlWhere :: QueryContext -> QueryParams -> String
timestampFiltersToSqlWhere ctx params  = case ctx.engine of
  Redshift -> 
              ctx.timestampColumn.name <#> ">=" <#> (redshiftConvertTimezone ctx.timestampColumn.timezone params.timezone params.dateFrom) <#> "AND" <#> 
              ctx.timestampColumn.name <#> "<"  <#> (redshiftConvertTimezone ctx.timestampColumn.timezone params.timezone params.dateTo)
  PostgreSql -> 
              -- I do not support cases where the dates in the table are stored in a timestamp without time zone column and the stored dates are not in UTC 
              -- Meaning: context = WithoutTimezone for dbTimezone which is not UTC
              -- This is an easy fix, PRs are welcome
              ctx.timestampColumn.name <#> ">=" <#> (psqlConvertTimezone params.timezone params.dateFrom) <#> "AND" <#> 
              ctx.timestampColumn.name <#> "<"  <#> (psqlConvertTimezone params.timezone params.dateTo)
  where
    redshiftConvertTimezone :: TimezoneInfo -> Number -> String -> String
    redshiftConvertTimezone (WithoutTimezone dbTimezone) userTimezone date = "CONVERT_TIMEZONE('" <> showTimezone dbTimezone <> "', '" <> showTimezone userTimezone <> "', '" <> date <> "' :: timestamp)"
    redshiftConvertTimezone WithTimezone userTimezone date = psqlConvertTimezone userTimezone date 

    showTimezone :: Number -> String
    showTimezone tz = if tz > zeroN then "UTC+" <> show tz else if tz < zeroN then "UTC" <> show tz else "UTC"

    showReverseUserTimezone :: Number -> String
    showReverseUserTimezone = showTimezone <<< (negativeOneN * _)

    psqlConvertTimezone :: Number -> String -> String
    psqlConvertTimezone userTimezone date = "('" <> date <> "' :: timestamp at time zone '" <> showReverseUserTimezone userTimezone <> "')"

zeroN :: Number
zeroN = toNumber 0

negativeOneN :: Number
negativeOneN = toNumber (-1)




{--
filtersToSqlWhere ::  String -> QueryParams -> QueryContext -> String
filtersToSqlWhere indent p ctx = intercalate (newLine <> "AND ") (timeStr:rest)
  where
    alias' col = 
        case fromMaybe (Simple col) (Map.lookup col ctx.fieldMap) of
          Simple col' -> alias ctx col'
          Expr col' -> col'

    --alias ctx
    rest = filtersToSqls params ctx
    timeStr = "    " <> alias' q.timeColName <> " >= " <> addTimezone p.dateFrom <> newLine <> "AND " <> alias' q.timeColName <> " < " <> addTimezone p.dateTo
    newLine = "\n" <> indent
    tz = show $ floor $ toNumber(-1) * p.timezone -- TODO: floor i sa hack fro redshift
    addTimezone dateStr = case q.engine of 
        Redshift   -> "CONVERT_TIMEZONE('" <> tz <> "', 'UTC', '" <> dateStr <> "')"
        PostgreSql -> "'" <> dateStr <> "' :: timestamp AT TIME ZONE '" <> tz <> "' "    
--}