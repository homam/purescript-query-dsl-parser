module Test.ToSql where

import Data.Argonaut (decodeJson)
import Data.Argonaut as Argonaut
import Data.Either (Either(..), either)
import Data.Int (toNumber)
import Data.List ((:), fromFoldable, List(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console as Console
import Prelude (Unit, discard, identity, show, ($), (<$>))
import QueryStringPSQL.Context (LMapType(..), QueryContext, QueryEngine(..), TimezoneInfo(..))
import QueryStringPSQL.Params (FilterLang(..), FilterVal(..), LikePosition(..), QueryParams, SqlCol(..), UnboundedRangeOrdering(..), emptyBreakdownDetails, emptyFilters)
import QueryStringPSQL.Parser.Utils as U
import QueryStringPSQL.ToSql (timestampFiltersToSqlWhere, toSql)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner as Runner
import Text.Parsing.Parser (runParser)

main :: Effect Unit
main = do 
  -- Console.log $ show contextRedshift
  -- Console.log $ show $ Argonaut.stringify $ Argonaut.encodeJson contextRedshift
  let str = Argonaut.stringify $ Argonaut.encodeJson contextRedshift
  Console.log $ show str
  Console.log $ either identity show $ (decodeJson :: Argonaut.Json -> Either String QueryContext) <$> Argonaut.jsonParser """
      {
          "timestampColumn": {
              "timezone": {"values":[0], "tag":"WithoutTimezone"}
            , "name":"us.\"timestamp\""
          }
        , "tableAlias":"us"
        , "fieldMap":[]
        , "engine":{"values":[], "tag":"Redshift"}
      }
    """
    --- "{\"timestampColumn\":{\"timezone\":{\"values\":[0],\"tag\":\"WithoutTimezone\"},\"name\":\"us.\\\"timestamp\\\"\"},\"tableAlias\":\"us\",\"fieldMap\":[],\"engine\":{\"values\":[],\"tag\":\"Redshift\"}}"
  launchAff_ $ Runner.runSpec [consoleReporter] do 
    describe "Query DSL Parser" do
      it "should parse tuple of numbers (1,2)" do
        runParser "(1,2)" (U.tuple U.number U.number) `shouldEqual` (Right $ Tuple 1.0 2.0)
    describe "toSQL" do
      let countrySqlCol = SqlColNormal "country"
      let operatorCodeSqlCol = SqlColNormal "operator_code"
      let publisherIdJSONCol = SqlColJSON { colName: "query_string", jsonField: "publisher_id" }
      let fieldMapCountryToCountryCode = Map.fromFoldable ([Tuple "country" (Simple "country_code")])  
      let fieldMapCountryToCountryCodeNulls = Map.fromFoldable ([Tuple "country" (CastNulls "country_code")])
      let redshiftContextWithCountryCodeFieldMap = contextRedshift { fieldMap = fieldMapCountryToCountryCode}  

      describe "Timestamp Filters" do
        it "should work for PSQL Context WithoutTimezone" do 
          timestampFiltersToSqlWhere contextPSQL params `shouldEqual` "u.\"timestamp\" >= ('2019-12-03' :: timestamp at time zone 'UTC-2.0') AND u.\"timestamp\" < ('2019-12-04' :: timestamp at time zone 'UTC-2.0')"
        it "should work for PSQL Context WithTimezone" do 
          timestampFiltersToSqlWhere contextPSQLWithTimezone params `shouldEqual` "u.\"timestamp\" >= ('2019-12-03' :: timestamp at time zone 'UTC-2.0') AND u.\"timestamp\" < ('2019-12-04' :: timestamp at time zone 'UTC-2.0')"
        it "should work for Redshift Context WithoutTimezone" do 
          timestampFiltersToSqlWhere contextRedshift params `shouldEqual` "us.\"timestamp\" >= CONVERT_TIMEZONE('UTC', 'UTC+2.0', '2019-12-03' :: timestamp) AND us.\"timestamp\" < CONVERT_TIMEZONE('UTC', 'UTC+2.0', '2019-12-04' :: timestamp)"
      
      describe "Casting Columns" do
        let countrySqlColJson = SqlColJSON {colName: "traits", jsonField: "country" }
        
        it "should work for SqlColNormal" do
          toSql params contextRedshift countrySqlCol `shouldEqual` "us.\"country\"" 

        it "should work for SqlColJSON" do
          toSql params contextPSQLWithTimezone countrySqlColJson `shouldEqual` "u.\"traits\"->>'country'"  

        it "should work for SqlColNormal fieldMapped to CastNulls" do
          toSql params (contextPSQL { fieldMap = fieldMapCountryToCountryCodeNulls  }) countrySqlCol `shouldEqual` "coalesce(cast(u.\"country_code\" as varchar), 'Unknown')"

        it "should work for SqlColNormal fieldMapped to Expr" do
          toSql params (contextPSQL { fieldMap = Map.fromFoldable ([Tuple "country" (Expr "CASE WHEN us.country_code IN ('UK', 'GB') THEN 'GB' ELSE us.country_code")])  }) countrySqlCol `shouldEqual` "(CASE WHEN us.country_code IN ('UK', 'GB') THEN 'GB' ELSE us.country_code)"
       
      describe "Breakdown" do
        let singleBreakdownCountry = Tuple countrySqlCol emptyBreakdownDetails
        let singleBreakdownOperatorCode = Tuple operatorCodeSqlCol emptyBreakdownDetails
        let breakDown = singleBreakdownCountry : singleBreakdownOperatorCode : Nil
        let breakDownByHour = Tuple (SqlColNormal "$hour") emptyBreakdownDetails
        let breakDownByPublisherId = Tuple publisherIdJSONCol emptyBreakdownDetails
        it "should work for a simple SingleBreakdown" do
          toSql params redshiftContextWithCountryCodeFieldMap singleBreakdownCountry `shouldEqual` 
            "us.\"country_code\" AS \"d_country\""
        it "should work for a simple Breakdown" do
          toSql params redshiftContextWithCountryCodeFieldMap breakDown `shouldEqual`  
            "us.\"country_code\" AS \"d_country\",\n\
            \us.\"operator_code\" AS \"d_operator_code\""
        it "should work for Breakdown by $hour" do
          toSql params redshiftContextWithCountryCodeFieldMap breakDownByHour `shouldEqual`  
            "date_trunc('hour', us.\"timestamp\") AS \"d_$hour\""
        it "should work for Breakdown by JSON column" do
          toSql params contextPSQLWithTimezone breakDownByPublisherId `shouldEqual`  
            "u.\"query_string\"->>'publisher_id' AS \"d_query_string->publisher_id\""
        it "should work for Expr Map" do
          toSql params contextRedshift (Tuple (SqlColNormal "screen_size") emptyBreakdownDetails) `shouldEqual`  
            "(coalesce(cast(round(us.screen_width/ 50) :: Int * 50 as varchar) || 'X' || cast(round(us.screen_height/ 50) :: Int * 50 as varchar), 'Unknown')) AS \"d_screen_size\""

      describe "FilterLang" do
        let countryEqNL = Tuple countrySqlCol (FilterEq $ FilterValStr "NL")
        let countryInNLDE = Tuple countrySqlCol (FilterIn $ fromFoldable [FilterValStr "NL", FilterValStr "DE"] )
        let countryLike = Tuple countrySqlCol (FilterLike LikeBefore "E")
        let countryNotLike = Tuple countrySqlCol (FilterNot $ FilterIn $ fromFoldable [FilterValStr "NL", FilterValStr "DE"])
        let screenWidthRange = Tuple (SqlColNormal "screen_width") (FilterRange (FilterValUnquotedNumber 240.0) (FilterValUnquotedNumber 640.0))
        let screenWidthUnboundedRange = Tuple (SqlColNormal "screen_width") (FilterUnboundedRange GTE (FilterValUnquotedInt 240))
        it "should work for a simple FilterEq" do
          toSql params contextRedshift countryEqNL `shouldEqual` """us."country" = 'NL'"""
          toSql params redshiftContextWithCountryCodeFieldMap countryEqNL `shouldEqual` """us."country_code" = 'NL'"""
        it "should work for a simple FilterLike" do
          toSql params redshiftContextWithCountryCodeFieldMap countryLike `shouldEqual` """us."country_code" LIKE '%E'"""
        it "should work for a simpple FilterIn" do
          toSql params redshiftContextWithCountryCodeFieldMap countryInNLDE `shouldEqual` """us."country_code" = 'NL' OR us."country_code" = 'DE'"""
        it "should work for NOT" do
          toSql params redshiftContextWithCountryCodeFieldMap countryNotLike `shouldEqual` """NOT (us."country_code" = 'NL' OR us."country_code" = 'DE')"""
        it "should work for Range of Numbers" do
          toSql params redshiftContextWithCountryCodeFieldMap screenWidthRange `shouldEqual` """us."screen_width" >= 240.0 AND us."screen_width" < 640.0"""
        it "should work for Unboundedn Range" do
          toSql params redshiftContextWithCountryCodeFieldMap screenWidthUnboundedRange `shouldEqual` """us."screen_width" >= 240"""
      describe "Filters" do
        it "should work for empty Filters" do
          toSql params redshiftContextWithCountryCodeFieldMap emptyFilters `shouldEqual` "TRUE"


params :: QueryParams
params = { 
  timezone : toNumber 2,
  dateFrom : "2019-12-03",
  dateTo : "2019-12-04",
  breakdown : Nil,
  filters : Map.empty
}

contextPSQL :: QueryContext
contextPSQL = {
  timestampColumn : {
    name: "u.\"timestamp\"",
    timezone: WithoutTimezone (toNumber 0)
  },
  tableAlias : "u",
  fieldMap : Map.empty,
  engine : PostgreSql
}

contextPSQLWithTimezone :: QueryContext
contextPSQLWithTimezone = {
  timestampColumn : {
    name: "u.\"timestamp\"",
    timezone: WithTimezone
  },
  tableAlias : "u",
  fieldMap : Map.fromFoldable [
    Tuple "screen_size" (Expr "coalesce(cast(round(us.screen_width/ 50) :: Int * 50 as varchar) || 'X' || cast(round(us.screen_height/ 50) :: Int * 50 as varchar), 'Unknown')")
  ],
  engine : PostgreSql
}

contextRedshift :: QueryContext
contextRedshift = {
  timestampColumn : {
    name: "us.\"timestamp\"",
    timezone: WithoutTimezone (toNumber 0)
  },
  tableAlias : "us",
  fieldMap : Map.fromFoldable [
    Tuple "screen_size" (Expr "coalesce(cast(round(us.screen_width/ 50) :: Int * 50 as varchar) || 'X' || cast(round(us.screen_height/ 50) :: Int * 50 as varchar), 'Unknown')")
  ],
  engine : Redshift
}