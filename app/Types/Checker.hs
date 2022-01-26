{-# LANGUAGE OverloadedStrings, TupleSections, RecordWildCards #-}

module Types.Checker (
  typecheckProgram
                     ) where

import Control.Monad
import Data.Either
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import Errors
import Syntax.Declaration
import Syntax.Program
import Syntax.Expr

import Debug.Pretty.Simple

data Context = Context
  { subtypesGraph :: Graph
  , topTypesScope :: Map.Map Text Type
  }

-- NOTE(Maxime): optimise this
type Graph = Map.Map Type (Set.Set Type)

data Info
  = LT Type
  | GT Type
  | ConeI   Text Type
  | CoconeI Text Type
  deriving (Show, Eq, Ord)

data Type
  = Typed Expr
  | Infod [Info]
  | Any
  deriving (Show, Eq, Ord)

insertInto :: Graph -> Type -> Set.Set Type -> Graph
insertInto g name surtypes = 
  let 
    step              = Map.insertWith (<>) name surtypes g 
    updateSurtype h s = insertInto h name $ Map.findWithDefault Set.empty s h
   in foldl' updateSurtype step surtypes

combine :: Graph -> Graph -> Graph
combine a b = Map.mapWithKey (insertInto b) a
  & Map.map (Map.foldr (<>) Set.empty)

baseTypeGraph :: Graph
baseTypeGraph = Map.fromList
  [
  ]

makeTypeGraph :: Program -> Either CatrinaError Graph
makeTypeGraph _ = combine baseTypeGraph <$> pure Map.empty

makeScope :: Program -> Either CatrinaError (Map.Map Text Type)
makeScope p = programDeclarations p
  & mapMaybe getTuple
  & sequence
  & fmap Map.fromList
  where
    emptyCtx   = Context baseTypeGraph Map.empty
    getTuple (ArrowDeclaration _ n t _) = Just $ (n,) <$> foldType emptyCtx t
    getTuple _ = Nothing

-- FIXME(Maxime): cones and cocones
foldType :: Context -> Expr -> Either CatrinaError Type
foldType _ (Composition [ ]) = pure.Typed 
  $ BinaryExpression (OtherOp "->") (Identifier "x") (Identifier "x")
foldType c (Composition [x]) = foldType c x
foldType m (Composition (
  ( BinaryExpression (OtherOp "->") a b
  : BinaryExpression (OtherOp "->") c d
  : xs )))
    | isRight (isSubtype m (Typed c) (Typed b)) 
      = foldType m $ Composition (BinaryExpression (OtherOp "->") a d : xs)
    | otherwise = Left $ TypeNotMatching Unit Unit
foldType _ (IntLiteral _) = pure.Typed 
  $ BinaryExpression (OtherOp "->") Unit (Identifier "Int")
foldType _ x = pure.Typed $ x

-- FIXME(Maxime): cones and cocones
isSubtype :: Context -> Type -> Type -> Either CatrinaError ()
isSubtype ctx (Typed (BinaryExpression (OtherOp "->") a b))
              (Typed (BinaryExpression (OtherOp "->") c d))
                =  join (isSubtype ctx <$> foldType ctx c <*> foldType ctx a)
                *> join (isSubtype ctx <$> foldType ctx b <*> foldType ctx d)
isSubtype Context{..} a b
  | a == b    = pure ()
  | otherwise = 
    case elem b <$> Map.lookup a subtypesGraph of
      Nothing    -> pTraceShow (a, b) $ Left $ TypeNotFound Unit
      Just False -> Left $ TypeNotMatching Unit Unit
      Just True  -> pure ()


checkDeclaration :: Context -> Declaration -> Either CatrinaError ()
checkDeclaration _ ObjectDeclaration{} = pure ()
checkDeclaration c (ArrowDeclaration _ _ type' value') 
  = join $ isSubtype c <$> foldType c type' <*> foldType c value'

typecheckProgram :: Program -> Either CatrinaError ()
typecheckProgram p = foldl' proceed (pure ()) (programDeclarations p)
  where
    context = Context <$> makeTypeGraph p <*> makeScope p

    proceed oldResult decl
      =   oldResult
      *>  context
      >>= flip checkDeclaration decl
