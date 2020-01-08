module QueryStringPSQL.Parser.Utils where

import Data.Foldable

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.List (List, many)
import Data.Map as SM
import Data.Maybe (Maybe, maybe)
import Data.Number (fromString)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Prelude (class Monad, class Ord, bind, liftA1, map, pure, ($), (*>), (<$>), (<*), (<*>), (<<<), (<>), (=<<))
import Text.Parsing.Parser (ParserT, fail)
import Text.Parsing.Parser.Combinators (between, try)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (class StringLike, char, oneOf, string, whiteSpace)
import Text.Parsing.Parser.Token (GenLanguageDef(LanguageDef), GenTokenParser, LanguageDef, alphaNum, digit, makeTokenParser, unGenLanguageDef)

type StrMap a = SM.Map String a

fromCharArray :: forall f. Foldable f => f Char -> String
fromCharArray = foldMap (Str.singleton <<< Str.codePointFromChar)

parens :: forall m a. Monad m => ParserT String m a -> ParserT String m a
parens = between (string "(" *> whiteSpace) (whiteSpace <* string ")")

betweenLax :: forall m s a open close. Monad m => StringLike s => ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a
betweenLax before after = between (lax before) (lax after)

betweenOptional :: forall m s a open close. Monad m => StringLike s => ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a
betweenOptional before after p = betweenLax before after p <|> p

lax :: forall a m s. Monad m => StringLike s => ParserT s m a -> ParserT s m a
lax p = whiteSpace *> p <* whiteSpace

tuple :: forall a b. ParserT String Identity a -> ParserT String Identity b -> ParserT String Identity (Tuple a b)
tuple a b = queryParser.parens $ Tuple <$> (a <* lax queryParser.comma) <*> b

-- Parses '3_hello-24: 3736'
propTuple :: forall a b m s. Monad m => StringLike s => ParserT s m a -> ParserT s m b -> ParserT s m (Tuple a b)
propTuple a b = Tuple <$> (a <* lax (string ":")) <*> b

emptyPropTuple :: forall a b m s. Monad m => StringLike s => ParserT s m a -> ParserT s m b -> b -> ParserT s m (Tuple a b)
emptyPropTuple a b mb = do
  a' <- a
  b' <- try (lax (string ":") *> b) <|> pure mb
  pure $ Tuple a' b'
  -- Tuple <$> (a <* lax (string ":")) <*> b

list :: forall a. ParserT String Identity a -> ParserT String Identity (List a)
list p = queryParser.brackets ps <|> queryParser.parens ps where
  ps = queryParser.commaSep p

listFlex :: forall a. ParserT String Identity a -> ParserT String Identity (List a)
listFlex p = queryParser.brackets ps <|> queryParser.parens ps <|> ps where
  ps = queryParser.commaSep p
   
strMap :: forall a. ParserT String Identity a -> ParserT String Identity (StrMap a)
strMap = map SM.fromFoldable <<< listFlex <<< propTuple queryParser.identifier

kvMap :: forall v k. Ord k => ParserT String Identity k -> ParserT String Identity v -> ParserT String Identity (SM.Map k v)
kvMap k v = map SM.fromFoldable $ listFlex $ propTuple k v

emptyStrMap :: forall a. a -> ParserT String Identity a -> ParserT String Identity (StrMap a)
emptyStrMap def p = map SM.fromFoldable $ listFlex $ (try $ propTuple queryParser.identifier p) <|> (map toTuple queryParser.identifier)
  where toTuple x = Tuple x def

number :: ∀ m. Monad m ⇒ ParserT String m Number
number = (go <<< toEither <<< fromString <<< fromCharArray <<< fromFoldable) =<< (many digit)
  where
    toEither :: forall b. Maybe b -> Either String b
    toEither = maybe (Left "Error parsing String to Number") Right

    go (Left e) = fail e
    go (Right v) = pure v

queryParser :: GenTokenParser String Identity
queryParser = makeTokenParser javaStyle where
  javaStyle  :: LanguageDef
  javaStyle   = LanguageDef (unGenLanguageDef emptyDef)
                { commentStart    = "/*"
                , commentEnd      = "*/"
                , commentLine     = "//"
                , nestedComments  = false
                , identStart      = alphaNum <|> char '_' <|> char '$'
                , identLetter     = alphaNum <|> oneOf ['_', '-']
                , reservedNames   = []
                , reservedOpNames = []
                , caseSensitive   = false
                }

----

concatP :: ParserT String Identity String -> ParserT String Identity String -> ParserT String Identity String
concatP = lift2 (<>) -- <$> pa <*> pb 
infixr 4 concatP as <+>
