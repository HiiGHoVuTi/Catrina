module Syntax.Declaration (
  Declaration(..), declaration
                          ) where

import Data.Text
import Parsing
import Syntax.Expr
import Text.Parsec
import Text.Parsec.Token

data Declaration
  = ArrowDeclaration Text Text Expr Expr
  | ObjectDeclaration Text Text Expr
  deriving (Show)
  
arrow' :: Parser Declaration
arrow' = do
  reserved lexer "ar"
  cat <- identifier lexer
  name <- identifier lexer
  reservedOp lexer ":"
  type' <- expr
  reservedOp lexer "="
  ArrowDeclaration (pack cat) (pack name) type' <$> expr

arrowBase :: Parser Declaration
arrowBase = do
  reserved lexer "ar"
  name <- identifier lexer
  reservedOp lexer ":"
  type' <- expr
  reservedOp lexer "="
  ArrowDeclaration (pack "Base") (pack name) type' <$> expr

object :: Parser Declaration
object = do
  reserved lexer "ob"
  cat <- identifier lexer
  name <- identifier lexer
  reservedOp lexer "="
  ObjectDeclaration (pack cat) (pack name) <$> expr

declaration :: Parser Declaration
declaration  = (try arrowBase <|> arrow')
           <|> object
           <?> "declaration"

