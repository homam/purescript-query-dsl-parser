module QueryStringPSQL.Parser.BreakdownDetailsParser (
  breakdownDetailsParser, runBreakdownDetailsParser
) where

import Control.Alternative ((<|>))
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (const, map, ($), (<$>))
import QueryStringPSQL.Params (BreakdownDetails(..), Sort(..), SortOrder(..))
import QueryStringPSQL.Parser.FiltersParser (filtersParser)
import QueryStringPSQL.Parser.SqlColParser (sqlColParser)
import QueryStringPSQL.Parser.Utils (propTuple, queryParser, tuple)
import Text.Parsing.Parser (ParseError, ParserT, runParser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (string)


-- Parses "sales: D"
sortParser :: ParserT String Identity Sort
sortParser = map toSort $ 
    propTuple sqlColParser (map (const ASC) (string "A") <|> map (const DESC) (string "D"))
  where
    toSort (Tuple a b) = Sort {by: a, order: b}

-- Parses "(sales:A,[views:100,sales:1])"
breakdownDetailsParser :: ParserT String Identity BreakdownDetails -- (Tuple (Maybe Sort) (Maybe (SM.StrMap Int)))
breakdownDetailsParser = toDetails <$> (
            map (bimap Just Just) (try $ tuple sortParser filtersParser)
        <|> map toTuple (queryParser.parens sortParser)
      )
  where toTuple x = Tuple (Just x) Nothing
        toDetails (Tuple s v) = BreakdownDetails { sort: s, valuesFilter: v }


runBreakdownDetailsParser :: String → Either ParseError BreakdownDetails
runBreakdownDetailsParser s = runParser s breakdownDetailsParser