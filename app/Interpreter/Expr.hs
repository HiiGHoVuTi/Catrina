{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Interpreter.Expr (
  evalExpr
                        ) where

import Data.List
import qualified Data.Map as Map 
import Data.Text hiding (map, find, foldl, reverse)
import Interpreter.BuiltIn
import Interpreter.Util
import Syntax.Expr
import System.IO.Unsafe
import Types.Category

import Debug.Pretty.Simple

  {-
composeList :: Env -> [Expr] -> Expr
composeList e = foldl (composeE e) (Composition [])

composeE :: Env -> Expr -> Expr -> Expr

composeE c (Identifier i) b   = composeE c (getFunction c i) b
composeE c a   (Identifier i) = composeE c a (getFunction c i)
composeE _ Unit _             = Unit
composeE _ (Composition []) b = b
composeE _ a (Composition []) = a
composeE c (Composition xs) (Composition ys) = composeList c $ xs ++ ys
composeE c a (Composition xs) = composeList c $ a : xs
composeE c (Composition xs) b = composeList c $ xs ++ pure b
composeE c (Cone   ms) b = ms
  & Map.map (flip (composeE c) b)
  & Map.mapWithKey (\k e -> Composition [ ConeProperty k, e ])
  & Cone
composeE c (Cocone ms) b = ms
  & Map.map (flip (composeE c) b)
  & Map.mapWithKey (\k e -> Composition [ e, CoconeConstructor k ])
  & Cocone
composeE c (BinaryExpression op e1 e2) b
  = BinaryExpression op (composeE c e1 b) (composeE c e2 b)
composeE _ (UnaryExpression (OtherOp "'") e) _ = e
composeE c (FunctorApplication lhs rhs) b = FunctorApplication lhs (composeE c rhs b)
composeE _ a b = Composition [a, b]
-- composeE _ a b = pTraceShow (a, b) undefined
  -}


evalExpr :: Env -> Expr -> Value -> IO Value
evalExpr _ expr' VPlaceholder = pure (VExpr expr')
-- evalExpr c e    (VExpr e')    = pure.VExpr $ composeE c e e'
evalExpr env expr' input = unsafeInterleaveIO $
  case expr' of
    Unit -> pure VUnit
    
    IntLiteral num -> pure (VInt num)
    
    FloatLiteral num -> pure (VFloat num)

    CharLiteral c -> pure (VShort c)

    StringLiteral str -> let
        toList a c = Composition 
          [ Cone $ Map.fromList [("head", CharLiteral c), ("tail", a)]
          , CoconeConstructor "cons" 
          ]
        asList     = foldl toList (CoconeConstructor "empty") (reverse str)
      in evalExpr env asList input

    UnaryExpression (OtherOp "'") liftedExpr -> pure (VExpr liftedExpr)

    UnaryExpression (OtherOp ",'") a -> flip pTraceShow undefined $
      Cone . Map.fromList $
        [ ("_1", a)
        , ("_2", Composition [])
        ]

    UnaryExpression (OtherOp name) opr -> evalExpr env 
      (getFunction env name)    -- NOTE(Maxime): Unaries are just functions 
      =<<
      evalExpr env opr input    -- NOTE(Maxime): Evaluate the input to it
    
    -- NOTE(Maxime): Those have to be written here because of evalExpr
    BinaryExpression (OtherOp "$") a b -> do
      lhs <- unwrapVExpr <$> evalExpr env a input
      rhs <- evalExpr env b input
      evalExpr env lhs rhs


    BinaryExpression (OtherOp ":,") a b ->
      evalExpr env (Composition 
        [ Cone $ Map.fromList
                   [ ("head", a)
                   , ("tail", b)
                   ]
        , CoconeConstructor "cons"
        ]) input


    -- NOTE(Maxime): (a + b) f --> f a + f b --> { f a, f b } (+)
    BinaryExpression (OtherOp name) lhs rhs -> 
      evalExpr env 
      (getFunction env name)  . VCone
      =<<
        sequenceA (Map.fromList 
        [ ("_1", evalExpr env lhs input)
        , ("_2", evalExpr env rhs input)
        ])

    Identifier name -> evalExpr env (getFunction env name) input
 
    
    -- NOTE(Maxime): special case for cones, {xs} {; ys} == {xs ; ys}
    Composition (Cone m:Cone m':xs)
      | Map.null m -> do
        old <- evalExpr env (Cone m') input
        let saved = VCone . Map.union (unwrapVCone old) 
                  . unwrapVCone $ input
        evalExpr env (Composition xs) saved

    -- NOTE(Maxime): Identity function
    Composition [] -> pure input
    
    -- NOTE(Maxime): a b c $ x -> b c $ a $ x
    Composition (x:xs) -> evalExpr env (Composition xs) =<< evalExpr env x input
 
    Cone values -> VCone <$> sequenceA (Map.map (flip (evalExpr env) input) values)

    -- NOTE(Maxime): can use unsafe due to typecheck
    Cocone mappings -> 
      case input of
        VCocone (name, value) -> evalExpr env (mappings Map.! name) value
        w -> pTraceShow w undefined

    ConeProperty prop -> pure (unsafeGet prop input)
    CoconeConstructor name -> pure (VCocone (name, input))
    
    -- NOTE(Maxime): top case violates laws but is impossible
    ConeAnalysis [] -> pure input
    ConeAnalysis [prop] -> pure (analyse prop input)
    ConeAnalysis (p:ps) -> do
      VCocone (k, v) <- evalExpr env (ConeAnalysis [p]) input
      VCocone . (k, ) <$> evalExpr env (ConeAnalysis ps) v

    -- TODO(Maxime): fix functor application
    FunctorApplication functor mappedExpr -> let
        unwrapped = unwrapVExpr <$> evalExpr env mappedExpr input
        rewrapped = UnaryExpression (OtherOp "'") <$> unwrapped
      in case functor of
        Composition []  -> evalExpr env mappedExpr input
        Composition [x] -> flip (evalExpr env) input . FunctorApplication x =<< rewrapped

        Identifier name' -> let
          functor' = FunctorApplication (getFunction env name') <$> rewrapped
                            in flip (evalExpr env) input =<< functor'

        Cone typeMap -> let
          VCone vcone = input
          -- combine v (UnaryExpression (OtherOp "(*)") t) =
          combine v t@(Composition []) =
            flip (evalExpr env) v . FunctorApplication t =<< unwrapped
          combine v t = flip (evalExpr env) v . FunctorApplication t =<< rewrapped
          distributed = Map.intersectionWith combine vcone typeMap
         in VCone <$> sequenceA distributed

        Cocone typeMap -> let
            VCocone (prop, val) = input
            functor'            = 
              case typeMap Map.! prop of
                -- UnaryExpression (OtherOp "(*)") t 
                t@(Composition [])
                  -> FunctorApplication t <$> unwrapped 
                t -> FunctorApplication t <$> rewrapped
          in VCocone . (prop, ) <$> (flip (evalExpr env) val =<< functor')

        Unit -> pure VUnit

        a -> pTraceShow (a, mappedExpr) undefined

    -- TODO(Maxime): refactor this stuff
    BuiltIn "lconcat" -> evalExpr env (Composition [ConeAnalysis ["_1","_2"],Cocone (Map.fromList [("cons",Cocone (Map.fromList [("cons",BinaryExpression (OtherOp ":,") (Composition [ConeProperty "_1",ConeProperty "head"]) (Composition [Cone (Map.fromList [("_1",Composition [ConeProperty "_1",ConeProperty "tail"]),("_2",Composition [ConeProperty "_2",CoconeConstructor "cons"])]),Identifier "lconcat"])),("empty",Composition [ConeProperty "_1",CoconeConstructor "cons"])])),("empty",Cocone (Map.fromList [("cons",Composition [ConeProperty "_2",CoconeConstructor "cons"]),("empty",Composition [Unit,CoconeConstructor "empty"])]))])]) input
    
    BuiltIn "lfold" -> evalExpr env (Composition [ConeAnalysis ["elems"],Cocone (Map.fromList [("cons",Composition [Composition [Cone (Map.fromList [("combine",Composition [ConeProperty "combine"]),("elems",Composition [ConeProperty "elems",ConeProperty "tail"]),("initial",BinaryExpression (OtherOp "$") (Composition [ConeProperty "combine"]) (Composition [Composition [Cone (Map.fromList [("new",Composition [ConeProperty "elems",ConeProperty "head"]),("old",Composition [ConeProperty "initial"])])]]))])],Identifier "lfold"]),("empty",Composition [ConeProperty "initial"])])]) input

    BuiltIn "strconcat" -> evalExpr env (Composition [Composition [Cone (Map.fromList [("combine",UnaryExpression (OtherOp "'") (Composition [Cone (Map.fromList [("_1",Composition [ConeProperty "old"]),("_2",Composition [ConeProperty "new"])]),Identifier "lconcat"])),("elems",Composition []),("initial",Composition [Unit,CoconeConstructor "empty"])])],Identifier "lfold"]) input
  
    BuiltIn name -> executeStd name input

getFunction :: Env -> Text -> Expr
getFunction env name = 
  case match env of
    Nothing       -> BuiltIn name
    Just (_, cat) -> snd $ arrows cat Map.! name
  where
    match = find (Map.member name . arrows . snd) . Map.toList

-- NOTE(Maxime): safe due to typecheck
unwrapVExpr :: Value -> Expr
unwrapVExpr (VExpr e) = e
unwrapVExpr _         = undefined

-- NOTE(Maxime): same as above
unwrapVCone :: Value -> Map.Map Text Value
unwrapVCone (VCone m) = m
unwrapVCone _         = undefined

-- NOTE(Maxime): error is mostly for debugging purposes
-- as those would be caught by the typechecker
unsafeGet :: Text -> Value -> Value
unsafeGet v (VCone a) = a Map.! v
unsafeGet name value  = error 
                      $ "Attempting to use ." 
                      <> unpack name 
                      <> " on non-cone: \n" 
                      <> show value

analyse :: Text -> Value -> Value
analyse name (VCone m) = let
  (VCocone (field, _)) = m Map.! name
  newmap               = updateUnwrapCocone name m
  in VCocone (field, VCone newmap)
analyse name value     = error 
                       $ "Attempting to use @" 
                       <> unpack name 
                       <> " on non-cone: \n" 
                       <> show value

