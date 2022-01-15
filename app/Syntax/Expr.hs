{-# LANGUAGE OverloadedStrings #-}

module Syntax.Expr (
  Expr(..), expr,
  OperatorToken(..)
                   ) where

import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Text hiding (map, scanl1, zip, reverse)
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
          | CharLiteral Char
          | StringLiteral String
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
  , [otherBinop ":," AssocRight]
  , [otherBinop  "$" AssocRight]
  , [ otherBinop "=="  AssocLeft, otherBinop "!="  AssocLeft 
    , otherBinop ">!=" AssocLeft, otherBinop "<!=" AssocLeft
    , otherBinop ">="  AssocLeft, otherBinop "<="  AssocLeft ]
  ]


unit' :: Parser Expr
unit' = Unit <$ braces lexer (oneOf "=" <|> pure '_') 
    <?> "unit"


sequencedCone :: Parser Expr
sequencedCone = do
  cones <- fmap (map Map.fromList) 
        $ braces    lexer 
        $ semiSep1  lexer 
        $ commaSep1 lexer 
        $ pair "=" expr
  let reseq = Composition 
            $ map Cone 
            $ scanl1 process cones
    in pure reseq
  where
    process :: Map.Map Text Expr -> Map.Map Text Expr -> Map.Map Text Expr
    process old new = Map.mapWithKey update (Map.union old new)
      where
        update k _ = if Map.member k new
                        then new Map.! k
                        else ConeProperty k

-- NOTE(Maxime): { a = b, c = d } or { a, b }
cone :: Parser Expr
cone = try sequencedCone
   <|> try (fmap (Cone . Map.fromList) . braces lexer . commaSep1 lexer $ pair "=" expr)
   <|> try (fmap (Cone . tuple)        . braces lexer . commaSep1 lexer $ expr)
   <?> "cone"

cocone :: Parser Expr
cocone = try (fmap (Cocone . Map.fromList) . brackets lexer . commaSep1 lexer $ pair "=" expr)
     <|> try (fmap (Cocone . tuple)        . brackets lexer . commaSep1 lexer $ expr)
     <?> "cocone"

coneProperty :: Parser Expr
coneProperty = fmap (ConeProperty . pack) $ char '.' *> identifier lexer

coneAnalysis :: Parser Expr
coneAnalysis = fmap (ConeAnalysis . pack) $ char '@' *> identifier lexer

-- FIXME
coconeConstructor :: Parser Expr
coconeConstructor = CoconeConstructor . pack <$> lexeme lexer (many1 letter <*  char '.')

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
    <|> try coneAnalysis
    <|> try literal

literal :: Parser Expr
literal = Identifier   . pack   <$> try (identifier    lexer)
      <|> FloatLiteral          <$> try (float         lexer)
      <|> IntLiteral            <$> try (natural       lexer)
      <|> CharLiteral           <$> try (charLiteral   lexer)
      <|> StringLiteral         <$>      stringLiteral lexer
      <?> "literal"

operation :: Parser Expr
operation = buildExpressionParser operatorsTable term

typeExpr :: Parser Expr
typeExpr = TypeExpr <$> try typeExpr'

expr :: Parser Expr
expr  = try typeExpr
    <|> try operation
    <?> "expression"

