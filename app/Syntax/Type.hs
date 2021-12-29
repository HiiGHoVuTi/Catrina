{-# LANGUAGE OverloadedStrings #-}
module Syntax.Type (
  Type(..), typeDecl
                   ) where


import qualified Data.Map as Map
import Data.Text hiding (reverse)
import Parsing
import Parsing.Operators
import Syntax.Common
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

newtype OperatorToken = OtherOp Text
  deriving Show

data Type = TUnit
          | TIdentifier Text
          | TArrow Type Type
          | TCone (Map.Map Text Type)
          | TCocone (Map.Map Text Type)
  deriving (Show)

unit' :: Parser Type
unit' = TUnit <$ braces lexer (oneOf ":" <|> pure '_') 

literal :: Parser Type
literal = TIdentifier . pack <$> try (identifier lexer)
      <?> "literal"

-- NOTE(Maxime): { a = b, c = d } or { a, b }
cone :: Parser Type
cone = try (fmap (TCone . Map.fromList) . braces lexer . commaSep1 lexer $ pair ":" typeDecl)
   <|> try (fmap (TCone . tuple)        . braces lexer . commaSep1 lexer $ typeDecl)
   <?> "cone"

cocone :: Parser Type
cocone = try (fmap (TCocone . Map.fromList) . brackets lexer . commaSep1 lexer $ pair ":" typeDecl)
     <|> try (fmap (TCocone . tuple)        . brackets lexer . commaSep1 lexer $ typeDecl)
     <?> "cocone"

term :: Parser Type
term =  try literal
    <|> try unit'
    <|> try cone
    <|> try cocone
    <|> parens lexer typeDecl

arrow :: Parser Type
arrow = buildExpressionParser [[binary "->" TArrow AssocRight]] term

typeDecl :: Parser Type
typeDecl =  try arrow
        <|> try literal
        <|> try unit'


