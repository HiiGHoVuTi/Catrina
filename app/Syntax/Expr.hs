{-# LANGUAGE OverloadedStrings,DeriveGeneric, DeriveAnyClass, PatternSynonyms, DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell, TypeFamilies #-}

module Syntax.Expr (
  Expr(..), ExprF(..), expr,
  OperatorToken(..), pattern Arrow, (-->)
                   ) where

import Control.DeepSeq
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.List
import qualified Data.Map as Map
import Data.Text hiding (map, scanl1, zip, reverse, transpose, foldl1', foldr)
import GHC.Generics (Generic)
import Parsing
import Parsing.Operators
import Syntax.Common
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Expr

pattern Arrow :: Expr -> Expr -> Expr
pattern Arrow t1 t2 = BinaryExpression (OtherOp "->") t1 t2

(-->) :: Expr -> Expr -> Expr
(-->) = Arrow

newtype OperatorToken = OtherOp Text
  deriving (Show, Eq, Generic, NFData, Ord)


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
          | FunctorApplication Expr Expr
          | ConeProperty Text
          | CoconeConstructor Text
          | ConeAnalysis [Text]
          | BuiltIn Text
  deriving (Show, Eq, Generic, NFData, Ord)

makeBaseFunctor ''Expr

otherPrefix :: String -> Operator Text () Identity Expr
otherPrefix name = prefix name (UnaryExpression (OtherOp $ pack name))

-- otherSuffix :: String -> Operator Text () Identity Expr
-- otherSuffix name = postfix name (UnaryExpression (OtherOp $ pack name))

otherBinop :: String -> Assoc -> Operator Text () Identity Expr
otherBinop name = binary name (BinaryExpression (OtherOp $ pack name))


-- FIXME(Maxime): Allow any op but still keep precedence
operatorsTable :: OperatorTable Text () Identity Expr
operatorsTable =
  [ -- [otherPrefix "'", otherPrefix "(*)"]
    [otherPrefix "-"]
  , [otherBinop  "*" AssocLeft , otherBinop  "/" AssocLeft]
  , [otherBinop  "+" AssocLeft , otherBinop  "-" AssocLeft]
  , [otherBinop ":," AssocRight]
  , [otherBinop  "$" AssocRight]
  , [ otherBinop "=="  AssocLeft, otherBinop "!="  AssocLeft 
    , otherBinop ">!=" AssocLeft, otherBinop "<!=" AssocLeft
    , otherBinop ">="  AssocLeft, otherBinop "<="  AssocLeft ]
  , [otherBinop ">>>" AssocRight]
  , [otherBinop "->" AssocRight]
  ]


unit' :: Parser Expr
unit' = Unit <$ braces lexer (oneOf ":" <|> pure '_') 
    <?> "unit"

-- NOTE(Maxime): { a: b, c: d ; a: .a + .c, e: f }
sequencedCone :: Parser Expr
sequencedCone = do
  cones <- fmap (map Map.fromList) 
        $ braces    lexer 
        $ semiSep1  lexer 
        $ commaSep  lexer 
        $ pair ":" expr
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

data StringLiteralPart = StringPart String | VariablePart Expr

combineList :: [Expr] -> Expr
combineList = foldr 
  (BinaryExpression (OtherOp ":,")) 
  (Composition [ Unit, CoconeConstructor "empty" ])

makeStringLit :: Text -> Expr
makeStringLit = convert . parse parser ""
  where
    parser :: Parser [StringLiteralPart]
    parser = many $ 
                 StringPart <$> many1 (noneOf "$")
        <|> fmap VariablePart   (char '$' *> char '(' *> expr <* char ')')
      
    convert :: Either ParseError [StringLiteralPart] -> Expr
    convert (Left e) = error $ show e
    convert (Right xs) = (\x -> Composition [x, Identifier "strconcat"]) 
      . combineList $ map toExpr xs

    toExpr (StringPart s)   = StringLiteral s
    toExpr (VariablePart e) = Composition [e, Identifier "show"]

-- NOTE(Maxime): #(a, b, c)
listLiteral :: Parser Expr
listLiteral 
  =  fmap combineList
  $  char '#' 
  *> parens lexer (commaSep lexer expr)

-- NOTE(Maxime): { a = b, c = d } or { a, b }
cone :: Parser Expr
cone = try sequencedCone
   <|> try (fmap (Cone . Map.fromList) . braces lexer . commaSep1 lexer $ pair ":" expr)
   <|> try (fmap (Cone . tuple)        . braces lexer . commaSep1 lexer $ expr)
   <?> "cone"


-- NOTE(Maxime): [ just;false : a, just;true : b, nothing; : c ]
sequencedCocone :: Parser Expr
sequencedCocone = do
  cocones <- fmap Map.fromList
    $ brackets   lexer
    $ commaSep1  lexer multiPair
  let reseq = process cocones
    in pure reseq
  where
    multiPair :: Parser ([Text], Expr)
    multiPair = do
      name <- map pack <$> semiSep1 lexer (identifier lexer <|> whiteSpace lexer $> "")
      reservedOp lexer ":"
      value <- expr
      pure(name, value)

    process :: Map.Map [Text] Expr -> Expr
    process m = Map.toList m
      & map toNested
      & foldl1' unify
    
    toNested :: ([Text], Expr) -> Expr
    toNested ([ ], v) = v
    toNested ([k], v) = Cocone $ Map.singleton k v
    toNested (k:r, v) = Cocone $ Map.singleton k $ toNested (r, v)

    unify :: Expr -> Expr -> Expr
    unify (Cocone m) (Cocone n) = Cocone (Map.unionWith unify m n)
    unify _ _ = undefined

cocone :: Parser Expr
cocone = try sequencedCocone
     <|> try (fmap (Cocone . Map.fromList) . brackets lexer . commaSep1 lexer $ pair ":" expr)
     <|> try (fmap (Cocone . tuple)        . brackets lexer . commaSep1 lexer $ expr)
     <?> "cocone"

coneProperty :: Parser Expr
coneProperty = fmap (ConeProperty . pack) $ char '.' *> identifier lexer

sequencedAnalysis :: Parser Expr
sequencedAnalysis = fmap (ConeAnalysis . map pack) 
                  $ char '@' *> semiSep1 lexer (identifier lexer)

coneAnalysis :: Parser Expr
coneAnalysis = try sequencedAnalysis
           <|> fmap (ConeAnalysis . pure . pack) (char '@' *> identifier lexer)

-- FIXME(Maxime): not only letter
coconeConstructor :: Parser Expr
coconeConstructor = CoconeConstructor . pack <$> lexeme lexer (many1 letter <*  char '.')

-- FIXME(Maxime): any expr, not just Identifier
functor :: Parser Expr
functor = do
  name <- identifier lexer
  FunctorApplication (Identifier $ pack name) <$> angles lexer operation

freeze :: Parser Expr
freeze = do
  e <- char '\'' *> singleTerm
  pure (UnaryExpression (OtherOp "'") e)

inject :: Parser Expr
inject = do
  e <- char '`' *> singleTerm
  pure (UnaryExpression (OtherOp "`") e)


backApply :: Parser Expr
backApply = do
  base <- pack <$> identifier lexer <* char '|'
  rest <- singleTerm
  pure (Composition [ rest, Identifier base ])

term :: Parser Expr
term =  fmap Composition (many singleTerm)

singleTerm :: Parser Expr
singleTerm 
      = try functor
    <|> try (parens lexer expr)
    <|> try unit'
    <|> try freeze
    <|> try inject
    <|> try backApply
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
      <|> CharLiteral           <$> try (char '#' *> charLiteral lexer)
      <|> makeStringLit . pack  <$>      stringLiteral lexer
      <|> listLiteral
      <?> "literal"

operation :: Parser Expr
operation = buildExpressionParser operatorsTable term

expr :: Parser Expr
expr  = operation
    <?> "expression"

