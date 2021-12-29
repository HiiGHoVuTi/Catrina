{-# LANGUAGE OverloadedStrings #-}
module Interpreter.Program (
  interpretProgram
                           ) where

import Data.Map hiding (foldl)
import Interpreter.Declaration
import Interpreter.Expr
import Interpreter.Value
import Syntax.Program
import Types.Category
import Types.Categories.Base

interpretProgram :: Program -> Value
interpretProgram (Program decls) = let
  start     = fromList . pure $ ("Base", base)
  scope     = foldl interpretDecl start decls
  (_, main) = arrows (scope ! "Base") ! "main"
                    in evalExpr scope main VUnit
