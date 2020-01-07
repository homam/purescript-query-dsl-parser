module QueryStringPSQL.Parser.SqlColParser (
  sqlColParser, runSqlColParser
) where

import Control.Alternative ((<|>))
import Data.Either (Either)
import Data.Identity (Identity)
import Prelude (bind, pure, ($), (<$>))
import QueryStringPSQL.Params (SqlCol(..))
import QueryStringPSQL.Parser.Utils (queryParser)
import Text.Parsing.Parser (ParseError, ParserT, runParser)
import Text.Parsing.Parser.Combinators (try)

sqlColParser :: ParserT String Identity SqlCol
sqlColParser = try parseJSONCol <|> parseNormalCol where
  parseJSONCol = do
    col <- queryParser.identifier
    _   <- queryParser.dot
    fld <- queryParser.identifier
    pure $ SqlColJSON {colName: col, jsonField: fld}
  parseNormalCol = SqlColNormal <$> queryParser.identifier

runSqlColParser :: String → Either ParseError SqlCol
runSqlColParser s = runParser s sqlColParser