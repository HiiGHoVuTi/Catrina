
module Semantics.Identifiers (
  identifierInScopeCheck
                             ) where

import Data.List
import qualified Data.Map as Map
import Data.Text (Text, toLower)
import Errors
import Semantics.Context
import Syntax.Declaration
import Syntax.Expr
import Syntax.Program

data TypeValueMode = TypeMode | ValueMode

identifierInScopeCheck :: Context -> Program -> Either CatrinaError ()
identifierInScopeCheck 
  Context{existingIdentifiers = ids} 
  Program{programDeclarations = xs}
    = foldl' (<*) (pure ()) $ map (checkDeclaration ids) xs


checkId :: TypeValueMode -> [Text] -> Text -> Either CatrinaError ()
checkId TypeMode _ n
  | toLower n == n = pure ()
  | otherwise      = Left (IdentifierNotInScope n)
checkId ValueMode ids n
  | n `elem` ids   = pure ()
  | otherwise      = Left (IdentifierNotInScope n)

checkExpr :: TypeValueMode -> [Text] -> Expr -> Either CatrinaError ()
checkExpr w ids (Composition es) = foldl' (<*) (pure ()) $ map (checkExpr w ids) es
checkExpr w ids (Cone m)         = Map.foldl' (<*) (pure ()) $ Map.map (checkExpr w ids) m
checkExpr w ids (Cocone m)       = Map.foldl' (<*) (pure ()) $ Map.map (checkExpr w ids) m
checkExpr w ids (UnaryExpression _ e)     = checkExpr w ids e
checkExpr w ids (BinaryExpression _ e e') = checkExpr w ids e *> checkExpr w ids e'
checkExpr w ids (FunctorApplication t e)  = checkExpr w ids t *> checkExpr w ids e
checkExpr w ids (Identifier n)   = checkId w ids n
checkExpr _ ___ ________________ = pure ()

checkDeclaration :: [Text] -> Declaration -> Either CatrinaError ()
checkDeclaration ids (ArrowDeclaration c _ t e) = do
  checkId   ValueMode ids c
  checkExpr TypeMode  ids t
  checkExpr ValueMode ids e
checkDeclaration ids (ObjectDeclaration c _ t) = do
  checkId   ValueMode ids c
  checkExpr ValueMode ids t

