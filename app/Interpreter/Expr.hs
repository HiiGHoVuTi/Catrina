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

evalExpr :: Env -> Expr -> Value -> IO Value
evalExpr _ expr' VPlaceholder = pure (VExpr expr')
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
 
    
    -- NOTE(Maxime): special case for cones
    Composition (Cone m:Cone m':xs)
      | Map.null m -> do
        new <- evalExpr env (Cone m') input
        let saved = VCone . Map.union (unwrapVCone input) 
                  . unwrapVCone $ new
        evalExpr env (Composition xs) saved
    

    -- NOTE(Maxime): Identity function
    Composition [] -> pure input
    
    -- NOTE(Maxime): a b c $ x -> b c $ a $ x
    Composition (x:xs) -> evalExpr env (Composition xs) =<< evalExpr env x input
 
    Cone values -> VCone <$> sequenceA (Map.map (flip (evalExpr env) input) values)

    -- NOTE(Maxime): can use unsafe due to typecheck
    -- FIXME(Maxime): wildcards won't save a match that has consumed stuff already
    Cocone mappings -> let 
      VCocone (name, value) = input
      matched               = 
        case Map.lookup name mappings of
          Just a  -> a
          Nothing -> mappings Map.! ""
     in evalExpr env matched value

    ConeProperty prop -> pure (unsafeGet prop input)
    CoconeConstructor name -> pure (VCocone (name, input))
    
    -- NOTE(Maxime): top case violates laws but is impossible
    ConeAnalysis [] -> pure input
    ConeAnalysis [prop] -> pure (analyse prop input)
    ConeAnalysis (p:ps) -> do
      VCocone (k, v) <- evalExpr env (ConeAnalysis [p]) input
      VCocone . (k, ) <$> evalExpr env (ConeAnalysis ps) v

    FunctorApplication functor mappedExpr ->
      case functor of
        Composition []  -> evalExpr env mappedExpr input
        Composition [x] -> evalExpr env (FunctorApplication x mappedExpr) input

        Identifier name' -> let
          functor' = FunctorApplication (getFunction env name') 
                   . unwrapVExpr <$> evalExpr env mappedExpr input
                             in flip (evalExpr env) input =<< functor'
        Cone typeMap -> let
          VCone vcone = input
          combine v t = evalExpr env (FunctorApplication t mappedExpr) v
          distributed = Map.intersectionWith combine vcone typeMap
         in VCone <$> sequenceA distributed

        Cocone typeMap -> let
            VCocone (prop, val) = input
            functor'            = FunctorApplication (typeMap Map.! prop) mappedExpr
          in VCocone . (prop, ) <$> evalExpr env functor' val

        _ -> undefined

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

