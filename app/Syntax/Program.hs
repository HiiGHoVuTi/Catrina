module Syntax.Program (
  Program(..), program
                      ) where

import Data.Text (Text, pack)
import Parsing
import Syntax.Declaration
import Text.Parsec
import Text.Parsec.Token

data Program = Program 
  { programHeader :: [Text]
  , imports :: [Text]
  , programDeclarations :: [Declaration]
  }
  deriving Show

program :: Parser Program
program = do
  whiteSpace lexer
  reserved lexer "import"
  imports' <- parens lexer (many $ pack <$> lexeme lexer (many1 $ letter <|> char '/'))
  
  whiteSpace lexer
  reserved lexer "export"
  exports' <- parens lexer (many $ pack <$> identifier lexer)
  
  whiteSpace lexer
  Program imports' exports' 
    <$> (whiteSpace lexer *> many (lexeme lexer declaration)) 
    <* eof

