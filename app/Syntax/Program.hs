module Syntax.Program (
  program
                      ) where

import Parsing
import Syntax.Expr

newtype Program = Program Expr
  deriving Show

-- FIXME(Maxime): obviously a program isn't a single expression
program :: Parser Program
program = Program <$> expr
