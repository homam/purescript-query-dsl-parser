module QueryStringPSQL.Parser.FilterLangParser where

import Control.Alternative ((<|>))
import Data.Either (Either)
import Data.Identity (Identity)
import Data.Tuple (Tuple(..), uncurry)
import Prelude ((*>), (<$>), ($>))
import QueryStringPSQL.Params (FilterLang(..), LikePosition(..))
import QueryStringPSQL.Parser.FilterValParser (filterValParser)
import QueryStringPSQL.Parser.Utils (list, parens, queryParser, tuple, (<+>))
import Text.Parsing.Parser (ParseError, ParserT, runParser)
import Text.Parsing.Parser.Combinators (try)



filterLangParser :: ParserT String Identity FilterLang
filterLangParser = go where
  go =
        FilterNot <$> (queryParser.symbol "NOT" *> (parens rest))
    <|> rest
  rest = 
        FilterIn <$> list (filterValParser true)
    <|> uncurry FilterRange <$> (queryParser.symbol "R" *> tuple (filterValParser false) (filterValParser false))
    <|> FilterEq <$> (filterValParser true)
    <|> uncurry FilterLike <$> (
          Tuple LikeBoth <$> try (star <+> queryParser.identifier <+> star)
      <|> Tuple LikeBefore <$> try (star <+> queryParser.identifier)
      <|> Tuple LikeAfter <$> try (queryParser.identifier <+> star)
    )
  star = queryParser.reservedOp "*" $> ""


runFilterLangParser :: String → Either ParseError FilterLang
runFilterLangParser s = runParser s filterLangParser