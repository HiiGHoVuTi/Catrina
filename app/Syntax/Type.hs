{-# LANGUAGE OverloadedStrings #-}
module Syntax.Type (
  Type(..), typeDecl,
  typeExpr'
                   ) where


import qualified Data.Map as Map
import Data.Text hiding (reverse)
import Parsing
import Parsing.Operators
import Syntax.Common
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

data Type = TUnit
          | TId
          | TIdentifier Text
          | TArrow Type Type
          | TCone (Map.Map Text Type)
          | TCocone (Map.Map Text Type)
          | TFunctor Type Type
  deriving (Show, Eq)

id' :: Parser Type
id' = TId <$ whiteSpace lexer

unit' :: Parser Type
unit' = TUnit <$ braces lexer (oneOf ":" <|> pure '_') 

tliteral :: Parser Type
tliteral = TIdentifier . pack <$> try (identifier lexer)
      <?> "type literal"

-- NOTE(Maxime): { a = b, c = d } or { a, b }
cone :: Parser Type
cone = try (fmap (TCone . Map.fromList) . braces lexer . commaSep1 lexer $ pair ":" arg)
   <|> try (fmap (TCone . tuple)        . braces lexer . commaSep1 lexer $ arg)
   <?> "cone"

cocone :: Parser Type
cocone = try (fmap (TCocone . Map.fromList) . brackets lexer . commaSep1 lexer $ pair ":" arg)
     <|> try (fmap (TCocone . tuple)        . brackets lexer . commaSep1 lexer $ arg)
     <?> "cocone"

functor :: Parser Type
functor = do
  name <- identifier lexer
  TFunctor (TIdentifier $ pack name) <$> angles lexer arg


arg :: Parser Type
arg = try typeDecl
  <|> try id'

term :: Parser Type
term =  try functor
    <|> try tliteral
    <|> try unit'
    <|> try cone
    <|> try cocone
    <|> try (parens lexer typeDecl)

arrow :: Parser Type
arrow = buildExpressionParser [[binary "->" TArrow AssocRight]] term

typeExpr' :: Parser Type
typeExpr' = try cone <|> try cocone

typeDecl :: Parser Type
typeDecl =  try arrow
        <|> try tliteral
        <|> try unit'


