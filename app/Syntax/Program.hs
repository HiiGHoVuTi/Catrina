module Syntax.Program (
  Program(..), program
                      ) where

import Parsing
import Syntax.Declaration
import Text.Parsec
import Text.Parsec.Token

newtype Program = Program { programDeclarations :: [Declaration] }
  deriving Show

-- FIXME(Maxime): obviously a program isn't a single expression
program :: Parser Program
program = Program <$> (whiteSpace lexer *> many (lexeme lexer declaration))
