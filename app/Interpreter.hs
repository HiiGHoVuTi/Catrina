module Interpreter (
  interpretProgram, start, interpretDecl, evalExpr, Value(VUnit),
  pShowValue, (#), Color(..)
                   ) where

import Interpreter.Declaration
import Interpreter.Expr
import Interpreter.PrettyShow
import Interpreter.Program
import Interpreter.Value
