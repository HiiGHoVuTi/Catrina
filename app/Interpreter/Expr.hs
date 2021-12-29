{-# LANGUAGE OverloadedStrings #-}
module Interpreter.Expr (
  evalExpr
                        ) where

-- FIXME(Maxime): implement laziness

import qualified Data.Map as Map 
import Data.Text hiding (map)
import Interpreter.BuiltIn
import Interpreter.Util
import Syntax.Expr
import Types.Category

evalExpr :: Env -> Expr -> Value -> Value
evalExpr env expr' input = 
  case expr' of
    Unit -> VUnit
    
    IntLiteral num -> VInt num
    
    FloatLiteral num -> VFloat num
    
    UnaryExpression (OtherOp name) opr -> evalExpr env 
      (getFunction env name)    -- NOTE(Maxime): Unaries are just functions 
      (evalExpr env opr input)  -- NOTE(Maxime): Evaluate the input to it
    
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

    BuiltIn name -> executeStd name input

-- FIXME(Maxime): actually look at the other categories
getFunction :: Env -> Text -> Expr
getFunction env name = if name `Map.member` arrows (env Map.! "Base")
                          then snd $ arrows (env Map.! "Base") Map.! name
                          else BuiltIn name
