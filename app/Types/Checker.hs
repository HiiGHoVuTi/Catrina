{-# LANGUAGE OverloadedStrings, TupleSections, RecordWildCards, LambdaCase, DeriveGeneric, TypeApplications, DataKinds, FlexibleContexts, PatternSynonyms, BangPatterns, ViewPatterns, FlexibleInstances, GADTs #-}

module Types.Checker (
  typecheckProgram
                     ) where

import Control.Monad
import qualified Control.Monad.State.Lazy as CMSL
import Control.Monad.Trans.State.Lazy
import Data.Either
import Data.Foldable
import Data.Function
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
  { typeScope         :: !(Map.Map T.Text Expr)
  , originalScope     :: Context
  , surtypesMap       :: Map.Map Expr (Set.Set Expr)
  , strictSubtypesMap :: Map.Map Expr (Set.Set Expr)
  , uids              :: [T.Text]
  }
  deriving Generic

pattern External :: Expr
pattern External <- 
  Composition [ Composition 
    [ BinaryExpression (OtherOp ":,") (StringLiteral _) _
    , _
    ], Identifier _]

type CheckerM = StateT Context (Either CatrinaError)

-- TODO(Maxime): improve error messages
quit :: HasCallStack => CatrinaError -> CheckerM a
quit = CMSL.lift . Left

isn'tPolymorphic :: Expr -> Bool
isn'tPolymorphic (Identifier t) = T.toLower t /= t
isn'tPolymorphic _ = True

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

isAdHoc :: Expr -> Bool
isAdHoc (Identifier t) | T.toLower t == t && not ("*" `T.isPrefixOf` t)
  = True
isAdHoc _ = False

compose2 :: Map.Map T.Text Expr -> Expr -> Expr -> Expr
compose2 scope t = cata go
  where
      prims = ["Char", "Int", "Float"] 
    -- NOTE(Maxime): incomplete ?
      go (CompositionF []) = t
      go (CompositionF (Identifier z:zs))
        | z `elem` prims = Composition $ Identifier z : zs
        | otherwise      = Composition (t:zs)
      go (IdentifierF f')
        | f' `elem` prims = Identifier f'
        | f' `Map.member` scope = Composition [t, case scope Map.! f' of
                                                    External -> Identifier f'
                                                    x -> x
                                              ]
      go (ConePropertyF n) = let
          Cone m = t
        in m Map.! n
      go x = embed x


isSubtype :: HasCallStack => Expr -> Expr -> CheckerM ()

-- trivial case
isSubtype a b
  | a == b = pure ()

-- functions
isSubtype (i1 `Arrow` o1) (i2 `Arrow` o2) = i2 `isSubtype` i1 *> o1 `isSubtype` o2

-- identity
isSubtype (Composition []) (t1 `Arrow` t2) = t2 `isSubtype` t1
-- const
isSubtype (Identifier "const") b = do
  t <- grabPlaceholder ; a <- grabPlaceholder
  (t `Arrow` (a `Arrow` t)) `isSubtype` b
isSubtype (Identifier "fmap") b = do
  x <- grabPlaceholder ; y <- grabPlaceholder
  f <- grabPlaceholder
  let
    c = Cone $ Map.fromList
      [ ("_1", f)
      , ("_2", Composition [x, f])
      , ("_3", x `Arrow` y)
      ]
  b `isSubtype` (c `Arrow` Composition [y, f])
isSubtype (Identifier "para") b = do
  pure () -- FIXME

isSubtype a (Composition [b]) = a `isSubtype` b -- NOTE(Maxime): shouldn't happen

-- ad-hoc polymorphism case
isSubtype a b
  | isAdHoc a && isAdHoc b = do
    subs <- checkSubtypeMap a
    case subs of
      Nothing -> quit TypeNotFound
      Just sb -> traverse_ (`isSubtype` b) sb

    surs <- checkSurtypeMap b
    case surs of
      Nothing -> quit TypeNotFound
      Just sr -> traverse_ (a `isSubtype`) sr

    originals <- get <&> surtypesMap.originalScope
    when (a `Map.notMember` originals) $ 
      modify $ field @"surtypesMap"       %~ Map.update (Just. Set.insert b) a
    when (b `Map.notMember` originals) $ 
      modify $ field @"strictSubtypesMap" %~ Map.alter (alterInsert a) b

isSubtype i b
  | isAdHoc i = do
    subs <- checkSubtypeMap i
    case subs of
      Nothing -> quit TypeNotFound
      Just sb -> traverse_ (`isSubtype` b) sb

    originals <- get <&> surtypesMap.originalScope
    when (i `Map.notMember` originals) $ 
      modify $ field @"surtypesMap"       %~ Map.update (Just. Set.insert b) i
    modify   $ field @"strictSubtypesMap" %~ Map.alter (alterInsert i) b

isSubtype a i 
  | isAdHoc i = do
    surs <- checkSurtypeMap i
    case surs of
      Nothing -> quit TypeNotFound -- cannot happen
      Just sr -> traverse_ (a `isSubtype`) sr

    originals <- get <&> surtypesMap.originalScope
    modify   $ field @"surtypesMap"       %~ Map.alter (alterInsert i) a
    when (i `Map.notMember` originals) $
      modify $ field @"strictSubtypesMap" %~ Map.alter (alterInsert a) i

-- parametric polymorphism case
isSubtype _ (Identifier tb)
  | "*" `T.isPrefixOf` tb = quit TypeNotMatching
isSubtype (Identifier ta) _
  | "*" `T.isPrefixOf` ta = pure ()

isSubtype (Composition as) (Composition bs) = zipWithM_ isSubtype as bs
isSubtype a (Composition xs) = do
  scope <- get <&> typeScope
  a `isSubtype` foldl1 (compose2 scope) xs
isSubtype (Composition xs) b = do
  scope <- get <&> typeScope
  foldl1 (compose2 scope) xs `isSubtype` b

-- products
isSubtype (Cone m) (Cone m') = do
  if all (`Map.member` m) (Map.keys m')
     then pure () 
     else quit TypeNotMatching

  sequence_ $ Map.intersectionWith isSubtype m m'

-- coproducts
isSubtype (Cocone m) (Cocone m') = do
  if all (`Map.member` m') (Map.keys m)
     then pure ()
     else quit TypeNotMatching

  sequence_ $ Map.intersectionWith isSubtype m m'

-- functors
isSubtype (FunctorApplication t  a)
          (FunctorApplication t' b)
            | t == t' = a `isSubtype` b

isSubtype a (FunctorApplication f x) =
  a `isSubtype` cata (go x) f
    where
      go t (CompositionF xs) = Composition (t:xs)
      go _ t = embed t

-- literals
isSubtype (IntLiteral    _) (_ `Arrow` t) = Identifier "Int"    `isSubtype` t
isSubtype (FloatLiteral  _) (_ `Arrow` t) = Identifier "Float"  `isSubtype` t
isSubtype (CharLiteral   _) (_ `Arrow` t) = Identifier "Char"   `isSubtype` t
isSubtype (StringLiteral _) (_ `Arrow` t) = Identifier "String" `isSubtype` t

-- TODO(Maxime): b shouldn't be Void
isSubtype Unit _ = pure ()

-- last resort, recursive traversal of the surtype map
isSubtype a b = anyIsRight
  [ surtypesMapCheck
  , typeScopeCheckA
  , typeScopeCheckB
  ]
  where
    isIdentifier (Identifier _) = True ; isIdentifier _ = False

    surtypesMapCheck = do
      sursM <- checkSurtypeMap a
      case sursM of
        Nothing   -> quit TypeNotMatching
        Just surs -> anyIsSubtype surs b

    typeScopeCheckA = do
      if isIdentifier a then pure () else quit TypeNotFound 

      sc <- get <&> typeScope
      let Identifier t = a
      if t `Map.member` sc
         then (sc Map.! t) `isSubtype` b
         else quit TypeNotFound

    typeScopeCheckB = do
      if isIdentifier b then pure () else quit TypeNotFound 

      sc <- get <&> typeScope
      let Identifier t = b
      if t `Map.member` sc
         then a `isSubtype` (sc Map.! t)
         else quit TypeNotFound

anyUnitTest :: [Either CatrinaError ()] -> Either CatrinaError ()
anyUnitTest = cata $ \case
  Nil -> Left TypeNotFound
  Cons (Left  _) xs -> xs
  Cons (Right _) __ -> Right ()

anyIsRight :: HasCallStack => [CheckerM ()] -> CheckerM ()
anyIsRight = lift' anyUnitTest
  where
    lift' :: Monad m => ([m a] -> m a) -> [StateT s m a] -> StateT s m a
    lift' f ss = StateT $ \s -> fmap (,s) $ f $ fmap (`evalStateT` s) ss


{- do -- undefined anyUnitTest 
  st <- get
  x
    & filter (isRight . flip evalStateT st)
    -- fail if no matches, succeed otherwise
    & \case
      [] -> quit TypeNotMatching
      _  -> pure ()
-}

anyIsSubtype :: HasCallStack => Set.Set Expr -> Expr -> CheckerM ()
anyIsSubtype (Set.toList -> []) _  = quit TypeNotFound
anyIsSubtype (Set.toList -> subs) b
  | b `elem` subs = pure ()
anyIsSubtype (Set.toList -> subs) b = do
    subs
      & filter isn'tPolymorphic
    -- try all subtypes and filter out failures
      & map (`isSubtype` b)   
      & anyIsRight

-- TODO(Maxime): bring in the interpreter (oops IO)
foldTopType :: Expr -> CheckerM Expr
foldTopType = cataA go
  where
    go :: ExprF (CheckerM Expr) -> CheckerM Expr
    go (CompositionF [x]) = x
    go (IdentifierF name) = do
      scope <- get <&> typeScope
      pure $ if name `Map.member` scope
         then case scope Map.! name of
                External -> Identifier name
                x -> x
         else Identifier name
    go other              = embed <$> sequence other


-- TODO(Maxime): refactor as para ?
foldBottomType :: Expr -> CheckerM Expr
-- FIXME
foldBottomType = dealWithInjections >=> dealWithInjections >=> cataA go
  where
    dealWithInjections :: Expr -> CheckerM Expr
    dealWithInjections = fmap embed . inj' . project

    inj' :: ExprF Expr -> CheckerM (ExprF Expr)
    inj' (UnaryExpressionF (OtherOp "'") r) = do
      i <- grabPlaceholder
      o <- flip cataA r $ \case
        UnaryExpressionF (OtherOp "`") e' -> do
          e <- foldBottomType =<< e'
          extraI <- grabPlaceholder ; function <- grabPlaceholder
          e `isSubtype` (extraI `Arrow` function)
          i `isSubtype` extraI
          pure function
        x -> embed <$> sequence x
      pure.project $ i `Arrow` o
    inj' e = traverse dealWithInjections e

    go (CompositionF thingies) = do
      -- Cannot be empty
      anyT <- grabPlaceholder
      rest <- sequence thingies <&> filter (/= Identifier "id")

      flip (`foldM` (anyT `Arrow` anyT)) rest $
        \f g -> do
          i <- grabPlaceholder ; x <- grabPlaceholder ; o <- grabPlaceholder
          f `isSubtype` (i `Arrow` x)
          g `isSubtype` (x `Arrow` o)
          pure $ i `Arrow` o

    go (UnaryExpressionF (OtherOp "-") r') = do
      r <- r'
      i <- grabPlaceholder
      r `isSubtype` (i `Arrow` Identifier "Float")
      pure $ i `Arrow` r

    go (BinaryExpressionF (OtherOp "$") a' b') = do
      a <- a' ; b <- b'
      input <- grabPlaceholder
      middle <- grabPlaceholder
      output <- grabPlaceholder
      a `isSubtype` (input `Arrow` (middle `Arrow` output))
      b `isSubtype` (input `Arrow` middle)
      pure (input `Arrow` output)

    go (BinaryExpressionF (OtherOp ":,") a' b') = do
      a <- a' ; b <- b'
      t <- grabPlaceholder ; i <- grabPlaceholder
      a `isSubtype` (i `Arrow` t)
      b `isSubtype` (i `Arrow` Composition [t, Identifier "List"])
      pure $ i `Arrow` Composition [t, Identifier "List"]

    go (BinaryExpressionF (OtherOp ">>>") f' g') = do
      f <- f' ; g <- g'
      i <- grabPlaceholder
      b <- grabPlaceholder ; c <- grabPlaceholder
      f `isSubtype` (i `Arrow` b)
      g `isSubtype` (i `Arrow` (b `Arrow` c))
      pure $ i `Arrow` c

    go (BinaryExpressionF (OtherOp o) a' b')
      | o `elem` ["+", "-", "*", "/"] = do
        a <- a' ; b <- b'
        input <- grabPlaceholder
        output <- grabPlaceholder
        output `isSubtype` Identifier "Float"
        a `isSubtype` (input `Arrow` output)
        b `isSubtype` (input `Arrow` output)
        pure (input `Arrow` output)
    
    go (BinaryExpressionF (OtherOp o) a' b')
      | o `elem` ["==", "!="] = do
        a <- a' ; b <- b'
        input <- grabPlaceholder
        middle <- grabPlaceholder
        output <- grabPlaceholder
        output `isSubtype` Identifier "Bool"
        a `isSubtype` (input `Arrow` middle)
        b `isSubtype` (input `Arrow` middle)
        pure (input `Arrow` output)

    go (BinaryExpressionF (OtherOp o) a' b')
      | o `elem` ["<=", ">=", ">!=", "<!="] = do
        a <- a' ; b <- b'
        input <- grabPlaceholder
        middle <- grabPlaceholder
        output <- grabPlaceholder
        middle `isSubtype` Identifier "Float"
        output `isSubtype` Identifier "Bool"
        a `isSubtype` (input `Arrow` middle)
        b `isSubtype` (input `Arrow` middle)
        pure (input `Arrow` output)

    go (ConeF m) = do
      input <- grabPlaceholder

      m' <- forM m $ \e' -> do
        e <- e'
        output <- grabPlaceholder
        e `isSubtype` (input `Arrow` output)
        pure output
      
      pure $ input `Arrow` Cone m'
    
    go (ConePropertyF t) = do
      returnType <- grabPlaceholder
      baseCone   <- grabPlaceholder

      -- { t: something, ... } .t
      baseCone `isSubtype` Cone (Map.singleton t returnType)

      pure $ baseCone `Arrow` returnType

    go (CoconeF m) = do
      output <- grabPlaceholder
      
      m' <- forM m $ \e' -> do
        e <- e'
        input <- grabPlaceholder
        e `isSubtype` (input `Arrow` output)
        pure input
      
      pure $ Cocone m' `Arrow` output
    
    go (CoconeConstructorF t) = do
      input  <- grabPlaceholder
      output <- grabPlaceholder
      
      Cocone (Map.singleton t input) `isSubtype` output

      pure $ input `Arrow` output

    -- TODO
    go (ConeAnalysisF ts) = do
      -- flip (`foldl` grabPlaceholder) ts $ \e t -> do
      --  undefined
      grabPlaceholder

    go (FunctorApplicationF f' m') = do
      f <- f' ; m <- m'

      a <- grabPlaceholder ; b <- grabPlaceholder
      m `isSubtype` (a `Arrow` (a `Arrow` b))

      pure $ Composition [a, f] `Arrow` Composition [b, f]

    go (IdentifierF "external") = Arrow <$> grabPlaceholder <*> grabPlaceholder
    go (IdentifierF t) = do
      typeScope <- get <&> typeScope
      case t `Map.lookup` typeScope of
        Nothing -> if T.toLower t /= t 
                      then quit TypeNotFound
                      else pure (Identifier t)
        Just ty -> foldTopType ty

    go other = embed <$> sequence other

makeScope :: Program -> CheckerM ()
makeScope p = do
  forM_ (programDeclarations p) $ \case
    -- NOTE(Maxime): just a patch
    ArrowDeclaration "Cat" name _ val' -> do
      type' <- foldTopType val'
      case val' of
        External -> pure()
        _ -> addToScope name type'
    ArrowDeclaration  _ name val' _ -> do
      type' <- foldTopType val'
      addToScope name type'
    ObjectDeclaration _ name val'   -> do
      type' <- foldTopType val'
      case val' of
        External -> pure()
        _ -> addToScope name type'
  sc <- get
  modify $ field @"originalScope" .~ sc
  where
    addToScope n t = modify $ field @"typeScope" %~ Map.insert n t

resetScope :: CheckerM ()
resetScope = do
  sc <- get <&> originalScope
  modify $ const sc
  modify $ field @"originalScope" .~ sc

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
                Identifier uid <- CMSL.lift grabPlaceholder
                -- parameter type marker
                modify $ Map.insert t uid
                pure   $ Identifier uid
              
              Just uid -> pure $ Identifier uid
        go other = embed <$> sequence other

checkStatements :: Program -> CheckerM ()
checkStatements p = do
  st <- get
  forM_ (programDeclarations p) $ \case
    ArrowDeclaration "Cat" _ _ _ -> pure ()
    ObjectDeclaration{} -> pure ()
    
    ArrowDeclaration _ name _ val' -> do
      let top = parametrise $ typeScope st Map.! name
      bottom <- foldBottomType val'
      bottom `isSubtype` top
      resetScope

  where
    parametrise :: Expr -> Expr
    parametrise = cata $ \case
      IdentifierF t | T.toLower t == t -> Identifier ("*" <> t)
      x -> embed x

typecheckProgram' :: Program -> CheckerM ()
typecheckProgram' p = do
  uip <- renamePhase p
  makeScope uip
  checkStatements uip

typecheckProgram :: Program -> Either CatrinaError ()
typecheckProgram p =
  evalStateT (typecheckProgram' p) Context
    { typeScope         = scope
    , originalScope     = undefined
    , surtypesMap       = surtypes
    , strictSubtypesMap = reverseMap surtypes
    -- T0, T1, ...
    , uids              = [0 :: Integer ..] & map (("_" <>).T.pack.show)
    }
  where
    bool  = Cocone $ Map.fromList [("true", Unit), ("false", Unit)]
    surtypes = Map.singleton (Identifier "Int") (Set.singleton (Identifier "Float"))
    scope = Map.fromList 
      [ ("Bool", bool)
      , ("String", Composition [Identifier "Char", Identifier "List"])
      -- built-in functions
      , ("strconcat", 
        Composition [Identifier "String", Identifier "List"]
        `Arrow` Identifier "String")
      ]




