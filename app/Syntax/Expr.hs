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
          | UnaryExpression OperatorToken Expr
        -- | Cone [(Text, Expr)]
        -- | Cocone [(Text, Expr)]
          | BinaryExpression OperatorToken Expr Expr
  deriving (Show)

-- FIXME(Maxime): Allow any op but still keep precedence
binary :: String -> (a -> a -> a) -> Assoc -> Operator Text () Identity a
binary  name fun = Infix   $ reservedOp lexer name $> fun
prefix, postfix :: String -> (a -> a) -> Operator Text () Identity a
prefix  name fun = Prefix  $ reservedOp lexer name $> fun
postfix name fun = Postfix $ reservedOp lexer name $> fun

otherPrefix :: String -> Operator Text () Identity Expr
otherPrefix name = prefix name (UnaryExpression (OtherOp $ pack name))

otherSuffix :: String -> Operator Text () Identity Expr
otherSuffix name = postfix name (UnaryExpression (OtherOp $ pack name))

otherBinop :: String -> Assoc -> Operator Text () Identity Expr
otherBinop name = binary name (BinaryExpression (OtherOp $ pack name))

operatorsTable :: OperatorTable Text () Identity Expr
operatorsTable =
  [ [otherSuffix "."          , otherPrefix "."] -- FIXME
  , [otherPrefix "-"]
  , [otherBinop  "*" AssocLeft, otherBinop  "/" AssocLeft]
  , [otherBinop  "+" AssocLeft, otherBinop  "-" AssocLeft]
  ]

-- NOTE(Maxime): operators have highest precedence, unsure about how good it is
composing :: Parser Expr -> Parser Expr
composing = fmap Composition . many1

literal :: Parser Expr
literal = Identifier   . pack   <$> try (identifier lexer)
      <|> FloatLiteral          <$> try (float      lexer)
      <|> IntLiteral            <$> try (natural    lexer)
      <?> "literal"

operation :: Parser Expr
operation = buildExpressionParser operatorsTable
          $  try (parens lexer expr)
         <|> try (composing literal)

expr :: Parser Expr
expr  =  composing
     (   try operation
     <|> try literal
     <?> "expression"
     )

