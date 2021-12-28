{-# LANGUAGE OverloadedStrings #-}

module Syntax.Expr (
  Expr(..), expr,
  OperatorToken(..)
                   ) where

import Data.Functor.Identity
import Data.Text hiding (zip, reverse)
import Parsing
import Parsing.Operators
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Expr

newtype OperatorToken = OtherOp Text
  deriving Show

data Expr = Unit
          | Composition [Expr]
          | Identifier   Text
          | IntLiteral   Integer
          | FloatLiteral Double
        -- FIXME(Maxime): String literals are a bit more complicated
          | UnaryExpression OperatorToken Expr
          | Cone [(Text, Expr)]
          | Cocone [(Text, Expr)]
          | BinaryExpression OperatorToken Expr Expr
          | BuiltIn Text
  deriving (Show)

otherPrefix :: String -> Operator Text () Identity Expr
otherPrefix name = prefix name (UnaryExpression (OtherOp $ pack name))

-- otherSuffix :: String -> Operator Text () Identity Expr
-- otherSuffix name = postfix name (UnaryExpression (OtherOp $ pack name))

otherBinop :: String -> Assoc -> Operator Text () Identity Expr
otherBinop name = binary name (BinaryExpression (OtherOp $ pack name))


-- FIXME(Maxime): Allow any op but still keep precedence
operatorsTable :: OperatorTable Text () Identity Expr
operatorsTable =
  [ [otherPrefix "-"]
  , [otherBinop  "*" AssocLeft, otherBinop  "/" AssocLeft]
  , [otherBinop  "+" AssocLeft, otherBinop  "-" AssocLeft]
  ]


unit' :: Parser Expr
unit' = Unit <$ braces lexer (oneOf " =")
    <?> "unit"


equalPair :: Parser (Text, Expr)
equalPair = do
  name <- pack <$> identifier lexer
  reservedOp lexer "="
  value <- expr
  pure(name, value)

-- NOTE(Maxime): maybe replace _1, _2 by 1, 2
tuple :: [Expr] -> [(Text, Expr)]
tuple = zip [(pack . ("_" <>) . show) n | n <- [1 :: Integer ..]]

-- NOTE(Maxime): { a = b, c = d } or { a, b }
cone :: Parser Expr
cone = try (fmap  Cone          . braces lexer . commaSep1 lexer $ equalPair)
   <|> try (fmap (Cone . tuple) . braces lexer . commaSep1 lexer $ expr)
   <?> "cone"

cocone :: Parser Expr
cocone = try (fmap  Cocone          . brackets lexer . commaSep1 lexer $ equalPair)
     <|> try (fmap (Cocone . tuple) . brackets lexer . commaSep1 lexer $ expr)
     <?> "cocone"


term :: Parser Expr
term =  fmap Composition . many
     $  try (parens lexer expr)
    <|> try unit'
    <|> try cone
    <|> try cocone
    <|> try literal

literal :: Parser Expr
literal = Identifier   . pack   <$> try (identifier lexer)
      <|> FloatLiteral          <$> try (float      lexer)
      <|> IntLiteral            <$> try (natural    lexer)
      <?> "literal"

operation :: Parser Expr
operation = buildExpressionParser operatorsTable term

expr :: Parser Expr
expr  =  try operation
     <|> try term
     <?> "expression"

