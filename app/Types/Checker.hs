{-# LANGUAGE OverloadedStrings, TupleSections, RecordWildCards, LambdaCase, DeriveGeneric, TypeApplications, DataKinds, FlexibleContexts, PatternSynonyms, BangPatterns, ViewPatterns, FlexibleInstances #-}

module Types.Checker (
  typecheckProgram
                     ) where

import Control.Monad
import qualified Control.Monad.State.Lazy as CMSL
import Control.Monad.Trans.State.Lazy
import Data.Either
import Data.Foldable
import Data.Functor.Foldable
import Data.Functor
import Data.Generics.Product
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics
import Lens.Micro
import Errors
import Syntax.Declaration
import Syntax.Program
import Syntax.Expr

import Debug.Pretty.Simple
import GHC.Stack

data Context = Context
  { typeScope         :: Map.Map T.Text Expr
  , surtypesMap       :: Map.Map Expr (Set.Set Expr)
  , strictSubtypesMap :: Map.Map Expr (Set.Set Expr)
  , uids              :: [T.Text]
  }
  deriving Generic

type CheckerM = StateT Context (Either CatrinaError)


quit :: HasCallStack => CatrinaError -> CheckerM a
quit = error "" -- CMSL.lift . Left

checkSurtypeMap :: Expr -> CheckerM (Maybe (Set.Set Expr))
checkSurtypeMap t = get <&> Map.lookup t . surtypesMap

checkSubtypeMap :: Expr -> CheckerM (Maybe (Set.Set Expr))
checkSubtypeMap t = get <&> Map.lookup t . strictSubtypesMap

reverseMap :: Map.Map Expr (Set.Set Expr) -> Map.Map Expr (Set.Set Expr)
reverseMap = Map.foldrWithKey combine Map.empty
  where 
    combine v (Set.toList -> [])   m = Map.insert v Set.empty m
    combine v (Set.toList -> k:ks) m = Map.alter (alterInsert v) k m 
      & combine v (Set.fromAscList ks)

alterInsert :: Ord a => a -> Maybe (Set.Set a) -> Maybe (Set.Set a)
alterInsert v Nothing   = Just (Set.singleton v)
alterInsert v (Just vs) = Just (Set.insert v vs)

grabUid :: CheckerM T.Text
grabUid = do
  -- get a uid and remove it from possibilities
  uid <- get <&> head . uids
  modify $ field @"uids" %~ tail
  pure uid

grabPlaceholder :: CheckerM Expr
grabPlaceholder = do
  uid <- grabUid
  let placeholder = Identifier uid
  modify $ field @"surtypesMap"       %~ Map.insert placeholder Set.empty
  modify $ field @"strictSubtypesMap" %~ Map.insert placeholder Set.empty
  pure placeholder

-- TODO(Maxime): all the laws, ex products, coproducts, operators
isSubtype :: HasCallStack => Expr -> Expr -> CheckerM ()

-- polymorphic case
isSubtype i@(Identifier t) b
  | T.toLower t == t = do
    subs <- checkSubtypeMap i
    case subs of
      Nothing -> quit TypeNotFound -- cannot happen
      Just sb -> traverse_ (`isSubtype` b) sb
    modify $ field @"surtypesMap" %~ Map.update (Just. Set.insert b) i
    modify $ field @"strictSubtypesMap" %~ Map.alter (alterInsert i) b

isSubtype a i@(Identifier t)
  | T.toLower t == t = do
    surs <- checkSurtypeMap i
    case surs of
      Nothing -> quit TypeNotFound -- cannot happen
      Just sr -> traverse_ (a `isSubtype`) sr
    modify $ field @"surtypesMap"       %~ Map.alter (alterInsert i) a
    modify $ field @"strictSubtypesMap" %~ Map.alter (alterInsert a) i

-- functions
isSubtype (i1 `Arrow` o1) (i2 `Arrow` o2) = i2 `isSubtype` i1 *> o1 `isSubtype` o2

-- identity
isSubtype (Composition []) (t1 `Arrow` t2) = t2 `isSubtype` t1

-- products
isSubtype (Cone m) (Cone m') = do
  if all (`Map.member` m) (Map.keys m')
     then pure () 
     else quit TypeNotMatching

  sequence_ $ Map.intersectionWith isSubtype m m'

-- literals
isSubtype (IntLiteral   _) (_ `Arrow` t) = Identifier "Int"   `isSubtype` t
isSubtype (FloatLiteral _) (_ `Arrow` t) = Identifier "Float" `isSubtype` t
isSubtype (CharLiteral  _) (_ `Arrow` t) = Identifier "Char"  `isSubtype` t

-- trivial case
isSubtype a b
  | a == b = pure ()

-- last resort, recursive traversal of the subtype map
isSubtype a b = do
  sursM <- checkSurtypeMap a
  case sursM of
    Nothing   -> quit TypeNotMatching
    Just surs -> anyIsSubtype surs b
 
anyIsSubtype :: HasCallStack => Set.Set Expr -> Expr -> CheckerM ()
anyIsSubtype (Set.toList -> []) _  = quit TypeNotFound
anyIsSubtype (Set.toList -> subs) b
  | b `elem` subs = pure ()
anyIsSubtype (Set.toList -> subs) b = do
    st <- get
    subs
      & filter isn'tPolymorphic
    -- try all subtypes and filter out failures
      & map (`isSubtype` b) & filter (isRight . flip evalStateT st)
    -- fail if no matches, succeed otherwise
      & \case
        [] -> quit TypeNotMatching
        _  -> pure ()
  where
    isn'tPolymorphic (Identifier t) = T.toLower t /= t
    isn'tPolymorphic _ = True

-- TODO(Maxime): bring in the interpreter (oops IO)
foldTopType :: Expr -> CheckerM Expr
foldTopType = cataA go
  where
    go :: ExprF (CheckerM Expr) -> CheckerM Expr
    go (CompositionF [x]) = foldTopType =<< x
    go other              = embed <$> sequence other


foldBottomType :: Expr -> CheckerM Expr
-- TODO(Maxime): add other folds (products and coproducts)
foldBottomType = cataA go
  where
    go (CompositionF [x]) = x
    
    go (CompositionF thingies) = do
      (a `Arrow` b : d `Arrow` c : xs) <- sequence thingies
      -- b < d
      b `isSubtype` d
      foldBottomType $ Composition $ a `Arrow` c : xs
    
    go (BinaryExpressionF (OtherOp o) a' b')
      | o `elem` ["+", "-", "*", "/"] = do
        a <- a' ; b <- b'
        input <- grabPlaceholder
        output <- grabPlaceholder
        output `isSubtype` Identifier "Float"
        a `isSubtype` (input `Arrow` output)
        b `isSubtype` (input `Arrow` output)
        pure (input `Arrow` output)
    
    go (ConeF m) = do
      input <- grabPlaceholder

      m' <- forM m $ \e' -> do
        e <- e'
        output <- grabPlaceholder
        -- ??
        e `isSubtype` (input `Arrow` output)
        pure output
      
      pure $ input `Arrow` Cone m'
    
    go (ConePropertyF t) = do
      returnType <- grabPlaceholder
      baseCone   <- grabPlaceholder

      -- { t: something, ... } .t
      baseCone `isSubtype` Cone (Map.fromList [(t, returnType)])

      pure $ baseCone `Arrow` returnType
    
    go (IdentifierF t)
      | T.toLower t /= t = do
      typeScope <- get <&> typeScope
      case t `Map.lookup` typeScope of
        Nothing -> quit TypeNotFound 
        Just ty -> foldTopType ty

    go other = embed <$> sequence other

makeScope :: Program -> CheckerM ()
makeScope p = do
  forM_ (programDeclarations p) $ \case
    ArrowDeclaration  _ name val' _ -> do
      type' <- foldTopType val'
      addToScope name type'
    ObjectDeclaration _ name val'   -> do
      type' <- foldTopType val'
      addToScope name type'
  where
    addToScope n t = modify $ field @"typeScope" %~ Map.insert n t

renamePhase :: Program -> CheckerM Program 
renamePhase (Program h i decls) = Program h i <$> traverse renameDecl decls
  where
    renameDecl :: Declaration -> CheckerM Declaration
    renameDecl (ArrowDeclaration c n top bottom) =
      ArrowDeclaration c n 
        <$> evalStateT (renameExpr top) Map.empty 
        <*> pure bottom
    renameDecl (ObjectDeclaration c n top) =
      ObjectDeclaration c n 
        <$> evalStateT (renameExpr top) Map.empty

    -- Okay I'm abusing the state monad
    
    renameExpr :: Expr -> StateT (Map.Map T.Text T.Text) CheckerM Expr
    renameExpr = cataA go
      where
        go (IdentifierF t)
          | T.toLower t == t = do
            zamap <- get
            case t `Map.lookup` zamap of
              -- if not already in the map grab a new uid
              Nothing -> do
                uid <- CMSL.lift grabUid
                modify $ Map.insert t uid
                pure   $ Identifier (t <> uid)
              
              Just uid -> pure $ Identifier (t <> uid)
        go other = embed <$> sequence other

checkStatements :: Program -> CheckerM ()
checkStatements p = do
  st <- get
  forM_ (programDeclarations p) $ \case
    ArrowDeclaration _ name _ val' -> do
      let top = typeScope st Map.! name
      bottom <- foldBottomType val'
      bottom `isSubtype` top
    ObjectDeclaration{} -> pure ()

typecheckProgram' :: Program -> CheckerM ()
typecheckProgram' p = do
  makeScope p
  uip <- renamePhase p
  checkStatements uip

typecheckProgram :: Program -> Either CatrinaError ()
typecheckProgram p = evalStateT (typecheckProgram' p) defaultContext

defaultContext :: Context
defaultContext = Context
    { typeScope         = Map.empty
    , surtypesMap       = surtypesMap
    , strictSubtypesMap = reverseMap surtypesMap
    -- T0, T1, ...
    , uids              = [0 :: Integer ..] & map (("_" <>).T.pack.show)
    }
    where
      surtypesMap = Map.fromList [(Identifier "Int", Set.fromList [Identifier "Float"])]

