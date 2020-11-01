module QueryStringPSQL.Parser.FilterLangParser where

import Control.Alternative ((<|>))
import Data.Either (Either)
import Data.Identity (Identity)
import Data.Tuple (Tuple(..), uncurry)
import Prelude ((*>), (<$>), ($>), (<$), (<*>), pure)
import QueryStringPSQL.Params (FilterLang(..), LikePosition(..), UnboundedRangeOrdering(..))
import QueryStringPSQL.Parser.FilterValParser (filterValParser)
import QueryStringPSQL.Parser.Utils (list, parens, queryParser, tuple, (<+>))
import Text.Parsing.Parser (ParseError, ParserT, runParser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (eof)


unboundedRangeOrderingParser :: ParserT String Identity UnboundedRangeOrdering
unboundedRangeOrderingParser = 
      queryParser.symbol "<=" $> LTE
  <|> queryParser.symbol "<"  $> LT
  <|> queryParser.symbol ">=" $> GTE
  <|> queryParser.symbol ">"  $> GT
  <|> queryParser.symbol "="  $> EQ

filterLangParser :: ParserT String Identity FilterLang
filterLangParser = go where
  go =
        FilterNot <$> (queryParser.symbol "NOT" *> (parens rest))
    <|> rest
  rest =
        FilterNone <$ (queryParser.symbol "-" *> eof)
    <|> FilterIn <$> list (filterValParser true)
    <|> uncurry FilterRange <$> (queryParser.symbol "R" *> tuple (filterValParser false) (filterValParser false))
    <|> FilterUnboundedRange <$> unboundedRangeOrderingParser <*> filterValParser false
    <|> uncurry FilterLike <$> (
          Tuple LikeAfter  <$> try (queryParser.identifier <+> star)
      <|> Tuple LikeBoth   <$> try (star <+> queryParser.identifier <+> star)
      <|> Tuple LikeBefore <$> try (star <+> queryParser.identifier)
    )
    <|> FilterIsNull <$ queryParser.reservedOp "$NULL"
    <|> FilterEq <$> (filterValParser true)
  star = queryParser.reservedOp "*" $> ""


runFilterLangParser :: String → Either ParseError FilterLang
runFilterLangParser s = runParser s filterLangParser

runUnboundedRangeOrderingParser :: String → Either ParseError UnboundedRangeOrdering
runUnboundedRangeOrderingParser s = runParser s unboundedRangeOrderingParser