{-# LANGUAGE OverloadedStrings, TupleSections, RecordWildCards, LambdaCase, DeriveGeneric, TypeApplications, DataKinds, FlexibleContexts, PatternSynonyms #-}

module Types.Checker (
  typecheckProgram
                     ) where

import Control.Monad
import qualified Control.Monad.State.Lazy as CMSL
import Control.Monad.Trans.State.Lazy
import Data.Either
import Data.Foldable
import Data.Functor
import Data.Generics.Product
import GHC.Generics
import Lens.Micro
import qualified Data.Map as Map
import qualified Data.Text as T
import Errors
import Syntax.Declaration
import Syntax.Program
import Syntax.Expr

import Debug.Trace

  {-
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
    | otherwise = Left TypeNotMatching

foldType _ (IntLiteral _) = pure.Typed 
  $ BinaryExpression (OtherOp "->") Unit (Identifier "Int")
foldType _ (FloatLiteral _) = pure.Typed 
  $ BinaryExpression (OtherOp "->") Unit (Identifier "Float")

-- (i -> Num) + (i -> Num) ---> i -> Num
foldType c (BinaryExpression (OtherOp op) a b)
  | op `elem` ["+", "-", "*", "/"] = do
  foldA <- foldType c a
  foldB <- foldType c b
 
  -- TODO(Maxime): guard that it is an arrow (i -> Num)
  case (isSubtype c foldA foldB, isSubtype c foldB foldA) of
    (Left _, Left _) -> Left TypeNotMatching 
    _                -> pure ()
  
  pure foldB

foldType _ x = pure.Typed $ x

-- FIXME(Maxime): cones and cocones
isSubtype :: Context -> Type -> Type -> Either CatrinaError ()
-- NOTE(Maxime): now that identifier is a subtype in the map
isSubtype _ (Typed (Identifier t)) _
  | toLower t == t = pure ()

-- NOTE(Maxime): Hacky
isSubtype _ (Typed Unit) _ = pure ()

isSubtype ctx (Typed (BinaryExpression (OtherOp "->") a b))
              (Typed (BinaryExpression (OtherOp "->") c d))
                =  join (isSubtype ctx <$> foldType ctx c <*> foldType ctx a)
                *> join (isSubtype ctx <$> foldType ctx b <*> foldType ctx d)
isSubtype Context{..} a b
  | a == b    = pure ()
  | otherwise =
    case elem b <$> Map.lookup a subtypesGraph of
      Nothing    -> pTraceShow (a, b) $ Left TypeNotFound
      Just False -> Left TypeNotMatching
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
-}

data Context = Context
  { typeScope   :: Map.Map T.Text Expr
  , surtypesMap :: Map.Map Expr [Expr]
  , uids        :: [T.Text]
  }
  deriving Generic

type CheckerM = StateT Context (Either CatrinaError)

quit :: CatrinaError -> CheckerM a
quit = CMSL.lift . Left

checkSurtypeMap :: Expr -> CheckerM (Maybe [Expr])
checkSurtypeMap t = get <&> Map.lookup t . surtypesMap


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
  modify $ field @"surtypesMap" %~ Map.insert placeholder []
  pure placeholder

-- TODO(Maxime): all the laws, ex products, coproducts, operators
isSubtype :: Expr -> Expr -> CheckerM ()

-- polymorphic case
isSubtype i@(Identifier t) b
  | T.toLower t == t = do
    -- FIXME
    modify $ field @"surtypesMap" %~ Map.update (Just.(b:)) i

isSubtype a i@(Identifier t)
  | T.toLower t == t = do
    surs <- checkSurtypeMap i
    case surs of
      Nothing -> quit TypeNotFound -- cannot happen
      Just sr -> traverse_ (a `isSubtype`) sr
    modify $ field @"surtypesMap" %~ Map.update (Just.(i:)) a

-- functions
isSubtype (i1 `Arrow` o1) (i2 `Arrow` o2) = isSubtype i2 i1 *> isSubtype o1 o2

-- identity
isSubtype (Composition []) (t1 `Arrow` t2) = t2 `isSubtype` t1

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
    Nothing   -> traceShow (a, b) $ quit TypeNotMatching
    Just surs -> anyIsSubtype surs b
 

-- TODO(Maxime): remove code duplication
anyIsSubtype :: [Expr] -> Expr -> CheckerM ()
anyIsSubtype [] _ = quit TypeNotMatching 
anyIsSubtype subs b
  | b `elem` subs = pure ()
  | otherwise     = do
    st <- get
    subs
    -- try all subtypes and filter out failures
      & map (`isSubtype` b) & filter (isRight . flip evalStateT st)
    -- fail if no matches, succeed otherwise
      & \case
        [] -> quit TypeNotMatching
        _  -> pure ()

subtypeOfAny :: Expr -> [Expr] -> CheckerM ()
subtypeOfAny _ [] = quit TypeNotMatching 
subtypeOfAny a surs
  | a `elem` surs = pure ()
  | otherwise     = do
    st <- get
    surs
    -- try all subtypes and filter out failures
      & map (a `isSubtype`) & filter (isRight . flip evalStateT st)
    -- fail if no matches, succeed otherwise
      & \case
        [] -> quit TypeNotMatching
        _  -> pure ()


-- TODO(Maxime): bring in the interpreter (oops IO)
foldTopType :: Expr -> CheckerM Expr
foldTopType (Composition [x]) = pure x
foldTopType e = pure e

foldBottomType :: Expr -> CheckerM Expr
-- TODO(Maxime): add other folds (products and coproducts)
foldBottomType (Composition [x]) = foldBottomType x

foldBottomType (Composition ( a `Arrow` b : d `Arrow` c : xs )) = do
    -- b < d
    join $ isSubtype <$> foldBottomType b <*> foldBottomType d
    foldBottomType $ Composition $ a `Arrow` c : xs

foldBottomType (BinaryExpression (OtherOp o) a b)
    | o `elem` ["+", "-", "*", "/"] = do
      input <- grabPlaceholder
      output <- grabPlaceholder
      a' <- foldBottomType a
      b' <- foldBottomType b
      isSubtype output (Identifier "Float")
      isSubtype a' (input `Arrow` output)
      isSubtype b' (input `Arrow` output)
      pure (input `Arrow` output)

foldBottomType e = pure e

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
    renameExpr (Identifier t)
      | T.toLower t == t = do
      zamap <- get
      case t `Map.lookup` zamap of
        -- if not already in the map grab a new uid
        Nothing -> do
          uid <- CMSL.lift grabUid
          modify $ Map.insert t uid
          pure   $ Identifier (t <> uid)
        
        Just uid -> pure $ Identifier (t <> uid)
    renameExpr (Composition xs) = Composition <$> traverse renameExpr xs
    renameExpr (UnaryExpression o e) = UnaryExpression o <$> renameExpr e
    renameExpr (BinaryExpression o a b) = BinaryExpression o <$> renameExpr a <*> renameExpr b
    renameExpr (Cone   m) = Cone   <$> traverse renameExpr m
    renameExpr (Cocone m) = Cocone <$> traverse renameExpr m
    renameExpr (FunctorApplication f x) = FunctorApplication <$> renameExpr f <*> renameExpr x
    renameExpr a = pure a


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
typecheckProgram p =
  evalStateT (typecheckProgram' p) Context
    { typeScope   = Map.empty 
    , surtypesMap = Map.fromList [(Identifier "Int", [Identifier "Float"])]
    -- T0, T1, ...
    , uids        = [0 :: Integer ..] & map (("_" <>).T.pack.show)
    }


