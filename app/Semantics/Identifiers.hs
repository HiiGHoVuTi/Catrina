
module Semantics.Identifiers (
  identifierInScopeCheck
                             ) where

import Data.List
import qualified Data.Map as Map
import Data.Text (Text)
import Errors
import Semantics.Context
import Syntax.Declaration
import Syntax.Expr
import Syntax.Program

identifierInScopeCheck :: Context -> Program -> Either CatrinaError ()
identifierInScopeCheck 
  Context{existingIdentifiers = ids} 
  Program{programDeclarations = xs}
    = foldl' (<*) (pure ()) $ map (checkDeclaration ids) xs


checkId :: [Text] -> Text -> Either CatrinaError ()
checkId ids n
  | n `elem` ids = pure ()
  | otherwise    = Left (IdentifierNotInScope n)

checkExpr :: [Text] -> Expr -> Either CatrinaError ()
checkExpr ids (Composition es) = foldl' (<*) (pure ()) $ map (checkExpr ids) es
checkExpr ids (Cone m)         = Map.foldl' (<*) (pure ()) $ Map.map (checkExpr ids) m
checkExpr ids (Cocone m)       = Map.foldl' (<*) (pure ()) $ Map.map (checkExpr ids) m
checkExpr ids (UnaryExpression _ e)     = checkExpr ids e
checkExpr ids (BinaryExpression _ e e') = checkExpr ids e *> checkExpr ids e'
checkExpr ids (FunctorApplication t e)  = checkExpr ids t *> checkExpr ids e
checkExpr ids (Identifier n)   = checkId ids n
checkExpr ___ ________________ = pure ()

checkDeclaration :: [Text] -> Declaration -> Either CatrinaError ()
checkDeclaration ids (ArrowDeclaration c _ t e) = do
  checkId   ids c
  checkExpr ids t
  checkExpr ids e
checkDeclaration ids (ObjectDeclaration c _ t) = do
  checkId   ids c
  checkExpr ids t

