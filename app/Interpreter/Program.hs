{-# LANGUAGE OverloadedStrings #-}
module Interpreter.Program (
  interpretProgram, start
                           ) where

import Data.Text (Text)
import Data.Map hiding (foldl)
import Interpreter.Declaration
import Interpreter.Expr
import Interpreter.Value
import Syntax.Program
import Types.Category
import Types.Categories.Base

start :: Map Text Category
start = fromList [("Base", base), ("Cat", catO'Cats)]

interpretProgram :: Program -> Value
interpretProgram (Program decls) = let
  scope     = foldl interpretDecl start decls
  (_, main) = arrows (scope ! "Base") ! "main"
                    in evalExpr scope main VUnit
