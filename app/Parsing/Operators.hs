module Parsing.Operators (
  binary, prefix, postfix
                        ) where


import Data.Functor
import Data.Functor.Identity
import Data.Text hiding (reverse)
import Parsing
import Text.Parsec.Token
import Text.Parsec.Expr

binary :: String -> (a -> a -> a) -> Assoc -> Operator Text () Identity a
binary  name fun = Infix   $ reservedOp lexer name $> fun
prefix, postfix :: String -> (a -> a) -> Operator Text () Identity a
prefix  name fun = Prefix  $ reservedOp lexer name $> fun
postfix name fun = Postfix $ reservedOp lexer name $> fun


