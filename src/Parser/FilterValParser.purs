module QueryStringPSQL.Parser.FilterValParser (
  filterValParser, runFilterValParser
) where

import Control.Alternative ((<|>))
import Data.Either (Either)
import Data.Identity (Identity)
import Prelude (class Category, identity, negate, pure, ($>), (*), (<$>), (<*), (<*>))
import QueryStringPSQL.Params (FilterVal(..))
import QueryStringPSQL.Parser.Utils (queryParser)
import Text.Parsing.Parser (ParseError, ParserT, runParser)
import Text.Parsing.Parser.Combinators (notFollowedBy, try)
import Text.Parsing.Parser.String (string)


filterValParser :: Boolean -> ParserT String Identity FilterVal
filterValParser requireSignedNums = 
      FilterValUnquotedInt <$> try (signInt <*> queryParser.natural <* notFollowedBy (string "."))
  <|> FilterValUnquotedNumber <$> (signNum <*> queryParser.float)
  <|> FilterValStr <$> queryParser.identifier

  where
    signInt = sign (\x -> -1 * x)
    signNum = sign (\x -> -1.0 * x)

    sign :: ∀ c a. Category c ⇒ c a a -> ParserT String Identity (c a a)
    sign f = 
      if requireSignedNums 
        then signed
        else signed <|> pure identity
      where
       signed = (queryParser.reservedOp "+" $> identity) <|> (queryParser.reservedOp "-" $> f)


runFilterValParser :: Boolean -> String → Either ParseError FilterVal
runFilterValParser requireSignedNums s = runParser s (filterValParser requireSignedNums)