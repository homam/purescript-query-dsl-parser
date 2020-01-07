module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.List (fromFoldable)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import QueryStringPSQL.Parser.Utils as U
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner as Runner
import Text.Parsing.Parser (ParseError, ParserT, runParser, fail)

main :: Effect Unit
main = launchAff_ $ Runner.run [consoleReporter] do 
  describe "tuple" do
    it "should parse tuple of numbers (1,2)" do
      runParser "(1,2)" (U.tuple U.number U.number) `shouldEqual` (Right $ Tuple 1.0 2.0)
  describe "list" do
    it "should parse list of numbers [1,2,3]" do
      runParser "[1,2,3]" (U.list U.number) `shouldEqual` (Right $ fromFoldable [1.0, 2.0, 3.0])
  describe "propTuple" do
    it "should parse propTuple hello: world" do
      runParser "hello: world" (U.propTuple U.queryParser.identifier U.queryParser.identifier) `shouldEqual` (Right $ Tuple "hello" "world")
  describe "purescript-spec" do
    describe "Attributes" do
      it "awesome" do
        let isAwesome = true
        isAwesome `shouldEqual` true
      pending "feature complete"
    describe "Features" do
      it "runs in NodeJS" $ pure unit
      it "runs in the browser" $ pure unit
      it "supports streaming reporters" $ pure unit
      it "supports async specs" do
        res <- delay (Milliseconds 100.0) *> pure "Alligator"
        res `shouldEqual` "Alligator"
      it "is PureScript 0.12.x compatible" $ pure unit


test :: Aff Unit
test = runParser "[1,2,3]" (U.list U.number) `shouldEqual` (Right $ fromFoldable [1.0, 2.0, 3.0])