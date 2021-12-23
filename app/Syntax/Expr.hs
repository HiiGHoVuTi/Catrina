{-# LANGUAGE OverloadedStrings #-}

module Syntax.Expr (
  Expr, expr
                   ) where
import Data.Functor
import Data.Functor.Identity
import Data.Text hiding (reverse)
import Parsing
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Expr

newtype OperatorToken = OtherOp Text
  deriving Show

data Expr = Composition [Expr]
          | Identifier   Text
          | IntLiteral   Integer
          | FloatLiteral Double
        -- NOTE(Maxime): String literals are a bit more complicated
        -- UnaryExpression OperatorToken Expr
        -- | Cone [(Text, Expr)]
        -- | Cocone [(Text, Expr)]
          | BinaryExpression OperatorToken Expr Expr
  deriving (Show)

-- FIXME(Maxime): types
binary  name fun = Infix   $ reservedOp lexer name $> fun
prefix  name fun = Prefix  $ reservedOp lexer name $> fun
postfix name fun = Postfix $ reservedOp lexer name $> fun

otherBinop :: String -> Assoc -> Operator Text () Identity Expr
otherBinop name = binary name (BinaryExpression (OtherOp $ pack name))

operatorsTable :: OperatorTable Text () Identity Expr
operatorsTable =
  [ [otherBinop "*" AssocLeft, otherBinop "/" AssocLeft]
  , [otherBinop "+" AssocLeft, otherBinop "-" AssocLeft]
  ]

-- NOTE(Maxime): operators have highest precedence, unsure about how good it is
composing :: Parser Expr -> Parser Expr
composing = fmap Composition . many1

literal :: Parser Expr
literal = Identifier   . pack   <$> try (identifier lexer)
      <|> FloatLiteral          <$> try (float      lexer)
      <|> IntLiteral            <$> try (integer    lexer)
      <?> "literal"

operation :: Parser Expr
operation = buildExpressionParser operatorsTable
          $  parens lexer expr
         <|> composing literal

expr :: Parser Expr
expr  =  composing
     (   try operation
     <|> try literal
     <?> "expression"
     )

