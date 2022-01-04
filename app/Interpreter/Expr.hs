{-# LANGUAGE OverloadedStrings #-}
module Interpreter.Expr (
  evalExpr
                        ) where

-- FIXME(Maxime): implement laziness

import Data.List
import qualified Data.Map as Map 
import Data.Text hiding (map, find)
import Interpreter.BuiltIn
import Interpreter.Util
import Syntax.Expr
import Syntax.Type
import Types.Category

evalExpr :: Env -> Expr -> Value -> Value
evalExpr _ expr' VPlaceholder = VExpr expr'
evalExpr env expr' input = 
  case expr' of
    Unit -> VUnit
    
    IntLiteral num -> VInt num
    
    FloatLiteral num -> VFloat num

    UnaryExpression (OtherOp "'") liftedExpr -> VExpr liftedExpr

    UnaryExpression (OtherOp name) opr -> evalExpr env 
      (getFunction env name)    -- NOTE(Maxime): Unaries are just functions 
      (evalExpr env opr input)  -- NOTE(Maxime): Evaluate the input to it
    
    -- NOTE(Maxime): Those have to be written here because of evalExpr
    BinaryExpression (OtherOp "$") a b ->
      evalExpr env (unwrapVExpr $ evalExpr env a input) (evalExpr env b input)

    BinaryExpression (OtherOp ":") a b ->
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
      (getFunction env name) 
      (VCone $ Map.fromList 
        [ ("_1", evalExpr env lhs input)
        , ("_2", evalExpr env rhs input)
        ])

    Identifier name -> evalExpr env (getFunction env name) input
    
    -- NOTE(Maxime): Identity function
    Composition [] -> input
    
    -- NOTE(Maxime): a b c $ x -> b c $ a $ x
    Composition (x:xs) -> evalExpr env (Composition xs) (evalExpr env x input)
 
    Cone mappings -> VCone . flip Map.map mappings $ \x -> 
      evalExpr env x input

    -- NOTE(Maxime): can use unsafe due to typecheck
    Cocone mappings -> let 
      VCocone (name, value) = input
      matched               = mappings Map.! name
     in evalExpr env matched value

    -- FIXME(Maxime): missing cases
    ConeProperty prop -> unsafeGet prop input
    CoconeConstructor name -> VCocone (name, input)
    ConeAnalysis prop -> analyse prop input

    TypeExpr type' -> VType type'

    FunctorApplication functor mappedExpr ->
      -- NOTE(Maxime): extracting type expression, only possibility for functors
      case functor of
        TId -> evalExpr env mappedExpr input
        TUnit -> VUnit 
        -- NOTE(Maxime): lookup and replace in functor expression
        TIdentifier name' -> let
            fromType (TypeExpr a) = a
            fromType _            = undefined
            functor'              = FunctorApplication 
              (fromType $ getFunction env name') mappedExpr
          in evalExpr env functor' input
        TArrow _ _ -> undefined
        TFunctor _ _ -> undefined -- FIXME(Maxime)?
        -- NOTE(Maxime): {x = a, y = b} F<{x: f, y: g}> -> {x: a F<f>, y: b F<g>}
        TCone typeMap -> let
            VCone vcone = input
            combine v t = evalExpr env (FunctorApplication t mappedExpr) v
            distributed = Map.intersectionWith combine vcone typeMap
           in VCone distributed
        
        TCocone typeMap -> let
            VCocone (prop, val) = input
            functor'            = FunctorApplication (typeMap Map.! prop) mappedExpr
          in evalExpr env functor' val

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

