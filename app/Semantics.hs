{-# LANGUAGE OverloadedStrings #-}

module Semantics (
  Context(..),
  runAllChecks, createContext
                 ) where

import Data.List
import qualified Data.Map as Map
import Errors
import Semantics.Context
import Semantics.Identifiers
import Syntax
import Syntax.Declaration
import Syntax.Expr

runAllChecks :: Context -> Program -> Either CatrinaError ()
runAllChecks = (foldl' (<*) (pure ()) .) . flip flip checks . (map .) . flip . flip id
  where
    checks = 
      [ identifierInScopeCheck
      , forbiddenConstructions
      ]


forbiddenConstructions :: Context -> Program -> Either CatrinaError ()
forbiddenConstructions ctx (Program _ _ ((ArrowDeclaration _ _ _ ex):xs)) = forbiddenConstructions' ex *> forbiddenConstructions ctx (Program [][]xs)
  where
    forbiddenConstructions' :: Expr -> Either CatrinaError ()
    forbiddenConstructions' (Composition es) = foldl' (<*) (pure ()) $ map forbiddenConstructions' es
    forbiddenConstructions' (Cone m)         = Map.foldl' (<*) (pure ()) $ Map.map forbiddenConstructions' m
    forbiddenConstructions' (Cocone m)       = Map.foldl' (<*) (pure ()) $ Map.map forbiddenConstructions' m
    forbiddenConstructions' (UnaryExpression _ e)     = forbiddenConstructions' e
    forbiddenConstructions' (BinaryExpression _ e e') = forbiddenConstructions' e *> forbiddenConstructions' e'
    forbiddenConstructions' (FunctorApplication _ e)  = forbiddenConstructions' e
    forbiddenConstructions' (ConeProperty "shell") = Left $ ForbiddenConstruction ".shell"
    forbiddenConstructions' _ = pure ()

forbiddenConstructions ctx (Program _ _ (_:xs)) = forbiddenConstructions ctx (Program [][]xs)
forbiddenConstructions _ _ = pure ()

