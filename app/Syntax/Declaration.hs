module Syntax.Declaration (
  Declaration(..), declaration
                          ) where

import Data.Text
import Parsing
import Syntax.Expr
import Syntax.Type
import Text.Parsec
import Text.Parsec.Token

data Declaration
  = ArrowDeclaration Text Text Type Expr
  | ObjectDeclaration Text Text Type
  deriving (Show)
  
arrow :: Parser Declaration
arrow = do
  reserved lexer "ar"
  cat <- identifier lexer
  name <- identifier lexer
  reservedOp lexer ":"
  type' <- typeDecl
  reservedOp lexer "="
  ArrowDeclaration (pack cat) (pack name) type' <$> expr

object :: Parser Declaration
object = do
  reserved lexer "ob"
  cat <- identifier lexer
  name <- identifier lexer
  reservedOp lexer "="
  ObjectDeclaration (pack cat) (pack name) <$> typeDecl

declaration :: Parser Declaration
declaration  = arrow
           <|> object
           <?> "declaration"

