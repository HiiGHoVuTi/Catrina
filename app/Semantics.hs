
module Semantics (
  runAllChecks, createContext
                 ) where

import Data.List
import Errors
import Semantics.Context
import Semantics.Identifiers
import Syntax

runAllChecks :: Context -> Program -> Either CatrinaError ()
runAllChecks = (foldl' (<*) (pure ()) .) . flip flip checks . (map .) . flip . flip id
  where
    checks = 
      [ identifierInScopeCheck
      ]
