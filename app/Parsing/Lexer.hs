module Parsing.Lexer (
  lexer
                     ) where

import Data.Functor.Identity
import Data.Text
import Data.Void
import Parsing.LangDef
import qualified Text.Parsec.Token as Tok


lexer :: Tok.GenTokenParser Text Void Identity
lexer = Tok.makeTokenParser catrinaDef 


