{-# LANGUAGE OverloadedStrings #-}

module Syntax.Expr (
  Expr(..), expr,
  OperatorToken(..)
                   ) where

import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Text hiding (zip, reverse)
import Parsing
import Parsing.Operators
import Syntax.Common
import Syntax.Type
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Expr

newtype OperatorToken = OtherOp Text
  deriving (Show, Eq)

data Expr = Unit
          | Composition [Expr]
          | Identifier   Text
          | IntLiteral   Integer
          | FloatLiteral Double
        -- FIXME(Maxime): String literals are a bit more complicated
          | UnaryExpression OperatorToken Expr
          | Cone (Map.Map Text Expr)
          | Cocone (Map.Map Text Expr)
          | BinaryExpression OperatorToken Expr Expr
          | FunctorApplication Type Expr
          | ConeProperty Text
          | CoconeConstructor Text
          | ConeAnalysis Text
          | TypeExpr Type
          | BuiltIn Text
  deriving (Show, Eq)

otherPrefix :: String -> Operator Text () Identity Expr
otherPrefix name = prefix name (UnaryExpression (OtherOp $ pack name))

-- otherSuffix :: String -> Operator Text () Identity Expr
-- otherSuffix name = postfix name (UnaryExpression (OtherOp $ pack name))

otherBinop :: String -> Assoc -> Operator Text () Identity Expr
otherBinop name = binary name (BinaryExpression (OtherOp $ pack name))


-- FIXME(Maxime): Allow any op but still keep precedence
operatorsTable :: OperatorTable Text () Identity Expr
operatorsTable =
  [ [otherPrefix "'"]
  , [otherPrefix "-"]
  , [otherBinop  "*" AssocLeft , otherBinop  "/" AssocLeft]
  , [otherBinop  "+" AssocLeft , otherBinop  "-" AssocLeft]
  , [otherBinop  "$" AssocRight]
  , [otherBinop "==" AssocLeft ]
  ]


unit' :: Parser Expr
unit' = Unit <$ braces lexer (oneOf "=" <|> pure '_') 
    <?> "unit"

-- NOTE(Maxime): { a = b, c = d } or { a, b }
cone :: Parser Expr
cone = try (fmap (Cone . Map.fromList) . braces lexer . commaSep1 lexer $ pair "=" expr)
   <|> try (fmap (Cone . tuple)        . braces lexer . commaSep1 lexer $ expr)
   <?> "cone"

cocone :: Parser Expr
cocone = try (fmap (Cocone . Map.fromList) . brackets lexer . commaSep1 lexer $ pair "=" expr)
     <|> try (fmap (Cocone . tuple)        . brackets lexer . commaSep1 lexer $ expr)
     <?> "cocone"

coneProperty :: Parser Expr
coneProperty = fmap (ConeProperty . pack) $ oneOf "." *> identifier lexer

coneAnalysis :: Parser Expr
coneAnalysis = fmap (ConeAnalysis . pack) $ oneOf "@" *> identifier lexer

coconeConstructor :: Parser Expr
coconeConstructor = fmap (CoconeConstructor . pack) $ identifier lexer <* lexeme lexer (oneOf ".")

functor :: Parser Expr
functor = do
  name <- identifier lexer
  FunctorApplication (TIdentifier $ pack name) <$> angles lexer operation

term :: Parser Expr
term =  fmap Composition . many
     $  try functor
    <|> try (parens lexer expr)
    <|> try unit'
    <|> try cone
    <|> try cocone
    <|> try coneProperty
    <|> try coconeConstructor
    <|> coneAnalysis
    <|> try literal

literal :: Parser Expr
literal = Identifier   . pack   <$> try (identifier lexer)
      <|> FloatLiteral          <$> try (float      lexer)
      <|> IntLiteral            <$> try (natural    lexer)
      <?> "literal"

operation :: Parser Expr
operation = buildExpressionParser operatorsTable term

typeExpr :: Parser Expr
typeExpr = TypeExpr <$> try typeExpr'

expr :: Parser Expr
expr  = try typeExpr
    <|> try operation
    <?> "expression"

