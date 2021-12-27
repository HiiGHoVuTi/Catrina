{-# LANGUAGE OverloadedStrings #-}
module Syntax.Type (
  Type(..), typeDecl
                   ) where


import Data.Text hiding (reverse)
import Parsing
import Parsing.Operators
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

newtype OperatorToken = OtherOp Text
  deriving Show

data Type = TUnit
          | TIdentifier Text
          | TArrow Type Type
  deriving (Show)

unit' :: Parser Type
unit' = TUnit <$ braces lexer (oneOf " :")

literal :: Parser Type
literal = TIdentifier . pack <$> try (identifier lexer)
      <?> "literal"

term :: Parser Type
term =  try literal
    <|> try unit'
    <|> parens lexer typeDecl

arrow :: Parser Type
arrow = buildExpressionParser [[binary "->" TArrow AssocRight]] term

typeDecl :: Parser Type
typeDecl =  try arrow
        <|> try literal
        <|> try unit'
