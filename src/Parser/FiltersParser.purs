module QueryStringPSQL.Parser.FiltersParser (
  filtersParser, runFiltersParser
) where

import Control.Alternative ((<|>))
import Data.Either (Either)
import Data.Identity (Identity)
import Data.Map as M
import Prelude (pure, (*>))
import QueryStringPSQL.Params (Filters)
import QueryStringPSQL.Parser.FilterLangParser (filterLangParser)
import QueryStringPSQL.Parser.SqlColParser (sqlColParser)
import QueryStringPSQL.Parser.Utils (kvMap)
import Text.Parsing.Parser (ParseError, ParserT, runParser)
import Text.Parsing.Parser.String (string)

filtersParser :: ParserT String Identity Filters
filtersParser = (string "-" *> pure M.empty) <|> kvMap sqlColParser filterLangParser

runFiltersParser :: String → Either ParseError Filters
runFiltersParser s = runParser s filtersParser