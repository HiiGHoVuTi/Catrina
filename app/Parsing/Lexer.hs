module Parsing.Lexer (
  lexer
                     ) where

import Data.Functor.Identity
import Data.Text
import Parsing.LangDef
import qualified Text.Parsec.Token as Tok

lexer :: Tok.GenTokenParser Text () Identity
lexer = Tok.makeTokenParser catrinaDef 


