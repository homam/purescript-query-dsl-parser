module Test.QueryStringParser where

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude (Unit, discard, ($))
import QueryStringPSQL.Params (BreakdownDetails(..), FilterLang(..), FilterVal(..), LikePosition(..), Sort(..), SortOrder(..), SqlCol(..), UnboundedRangeOrdering(..), emptyBreakdownDetails, emptyFilters)
import QueryStringPSQL.Parser.BreakdownDetailsParser (runBreakdownDetailsParser)
import QueryStringPSQL.Parser.BreakdownParser (runBreakdownParser)
import QueryStringPSQL.Parser.FilterLangParser (runFilterLangParser, runUnboundedRangeOrderingParser, unboundedRangeOrderingParser)
import QueryStringPSQL.Parser.FilterValParser (runFilterValParser)
import QueryStringPSQL.Parser.FiltersParser (runFiltersParser)
import QueryStringPSQL.Parser.SqlColParser (runSqlColParser)
import QueryStringPSQL.Parser.Utils as U
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner as Runner
import Text.Parsing.Parser (runParser)


main :: Effect Unit
main = do 
    --- "{\"timestampColumn\":{\"timezone\":{\"values\":[0],\"tag\":\"WithoutTimezone\"},\"name\":\"us.\\\"timestamp\\\"\"},\"tableAlias\":\"us\",\"fieldMap\":[],\"engine\":{\"values\":[],\"tag\":\"Redshift\"}}"
  let filterInAEDE = FilterIn $ FilterValStr "AE" : FilterValStr "DE" : Nil
  let filterLikeDMB = FilterLike LikeBefore "DMB"
  let filterLike1After = FilterLike LikeAfter "1"

  launchAff_ $ Runner.runSpec [consoleReporter] do 
    describe "Query DSL Parser" do
      it "should parse tuple of numbers (1,2)" do
        runParser "(1,2)" (U.tuple U.number U.number) `shouldEqual` (Right $ Tuple 1.0 2.0)
    describe "Parsing FilterVal" do
      it "should parse FilterValStr" do
        runFilterValParser false "AE" `shouldEqual` Right (FilterValStr "AE")
    describe "Parsing SqlCol" do
      it "should parse SqlColNormal" do
        runSqlColParser "country" `shouldEqual` (Right $ SqlColNormal "country")
      it "should parse SqlColJSON" do
        runSqlColParser "query.publisher-id" `shouldEqual` (Right $ SqlColJSON {colName: "query", jsonField: "publisher-id"})
    describe "Parsing FilterLang" do
      it "should parse FilterLangEq" do
        runFilterLangParser "AE" `shouldEqual` Right (FilterEq $ FilterValStr "AE")
        runFilterLangParser "[AE,DE]" `shouldEqual` Right filterInAEDE
        runFilterLangParser "[+240,+320]" `shouldEqual` Right (FilterIn $ FilterValUnquotedInt 240 : FilterValUnquotedInt 320 : Nil)
        runFilterLangParser "R(+240,+1024)" `shouldEqual` Right (FilterRange (FilterValUnquotedInt 240) (FilterValUnquotedInt 1024))
        runFilterLangParser "NOT(R(+240,+1024))" `shouldEqual` Right (FilterNot (FilterRange (FilterValUnquotedInt 240) (FilterValUnquotedInt 1024)))
      it "should parse FilterUnboundedRange" do
        runUnboundedRangeOrderingParser ">=" `shouldEqual` Right GTE
        runFilterLangParser ">= 240" `shouldEqual` Right (FilterUnboundedRange GTE (FilterValUnquotedInt 240))
        runFilterLangParser ">=240" `shouldEqual` Right (FilterUnboundedRange GTE (FilterValUnquotedInt 240))
        runFilterLangParser "<240" `shouldEqual` Right (FilterUnboundedRange LT (FilterValUnquotedInt 240))
      it "should parse FilterLike" do
        runFilterLangParser "*DMB" `shouldEqual` Right filterLikeDMB
        runFilterLangParser "1*" `shouldEqual` Right filterLike1After
    describe "Parsing Filters" do
      it "should parse simple Filters expression" do
        runFiltersParser "country:[AE,DE]" `shouldEqual` Right (Map.fromFoldable $ Tuple (SqlColNormal "country") filterInAEDE : Nil)
        runFiltersParser "country: (AE, DE)" `shouldEqual` Right (Map.fromFoldable $ Tuple (SqlColNormal "country") filterInAEDE : Nil)
        runFiltersParser "country: (AE, DE), affiliate: *DMB" `shouldEqual` 
            Right (Map.fromFoldable $ 
                Tuple (SqlColNormal "country") filterInAEDE
              : Tuple (SqlColNormal "affiliate") filterLikeDMB
              : Nil)
        runFiltersParser "affiliate: 1*" `shouldEqual` 
            Right (Map.fromFoldable $ 
                Tuple (SqlColNormal "affiliate") filterLike1After
              : Nil)
    describe "Parsing Breakdown" do
      let countryASalesGTE100BreakdownDetails = BreakdownDetails { 
                sort: Just $ Sort {by: SqlColNormal "country", order: ASC}, 
                valuesFilter: Just $ Map.fromFoldable $ [Tuple (SqlColNormal "sales") (FilterUnboundedRange GTE (FilterValUnquotedInt 100))]  
              }
      it "should parse BrekadownDetails" do
        runBreakdownDetailsParser "(sales:D)" `shouldEqual` 
            Right (BreakdownDetails { 
                sort: Just $ Sort {by: SqlColNormal "sales", order: DESC}, 
                valuesFilter: Nothing  
              })
        runBreakdownDetailsParser "(country:A, [sales:>=100])" `shouldEqual` 
            Right countryASalesGTE100BreakdownDetails
      it "should parse Brekadown" do
        runBreakdownParser "country:(country:A, [sales:>=100])" `shouldEqual`
            Right (Tuple (SqlColNormal "country") countryASalesGTE100BreakdownDetails : Nil)
        runBreakdownParser "country" `shouldEqual`
            Right (Tuple (SqlColNormal "country") emptyBreakdownDetails : Nil)
        runBreakdownParser "$hour" `shouldEqual`
            Right (Tuple (SqlColNormal "$hour") emptyBreakdownDetails : Nil)