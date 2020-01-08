module QueryStringPSQL.Parser.BreakdownParser (
  breakdownParser, runBreakdownParser
) where

import Control.Alternative ((<|>))
import Data.Either (Either)
import Data.Identity (Identity)
import Prelude (pure, (<*))
import QueryStringPSQL.Params (Breakdown, emptyBreakdownDetails)
import QueryStringPSQL.Parser.BreakdownDetailsParser (breakdownDetailsParser)
import QueryStringPSQL.Parser.SqlColParser (sqlColParser)
import QueryStringPSQL.Parser.Utils (emptyPropTuple, listFlex)
import Text.Parsing.Parser (ParseError, ParserT, runParser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (eof)


-- Parses "country_code:(sales:A),operator_code,date:(views:A,[sales:10,views:100])"
breakdownParser :: ParserT String Identity Breakdown
breakdownParser = listFlex (emptyPropTuple sqlColParser (try breakdownDetailsParser <|> pure emptyBreakdownDetails) emptyBreakdownDetails) <* eof


runBreakdownParser :: String → Either ParseError Breakdown
runBreakdownParser s = runParser s breakdownParser