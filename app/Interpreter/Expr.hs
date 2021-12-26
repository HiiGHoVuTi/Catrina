{-# LANGUAGE OverloadedStrings #-}
module Interpreter.Expr (
  evalExpr, Value(..)
                        ) where

-- FIXME(Maxime): implement laziness

import Data.Text
import Interpreter.BuiltIn
import Syntax.Expr
import Types.Category

type Env = [Category]

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
      (VCone 
        [ ("_1", evalExpr env lhs input)
        , ("_2", evalExpr env rhs input)
        ])

    Identifier name -> evalExpr env (getFunction env name) input
    
    -- NOTE(Maxime): Identity function
    Composition [] -> input
    
    -- NOTE(Maxime): a b c $ x -> b c $ a $ x
    Composition (x:xs) -> evalExpr env (Composition xs) (evalExpr env x input)
    
    BuiltIn name -> executeStd name input

-- FIXME(Maxime): actually look at the scope
getFunction :: Env -> Text -> Expr
getFunction _ = BuiltIn
