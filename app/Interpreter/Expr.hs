{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Interpreter.Expr (
  evalExpr
                        ) where

-- NOTE(Maxime): implement thunks ?

import Data.List
import qualified Data.Map as Map 
import Data.Text hiding (map, find, foldl, reverse)
import Interpreter.BuiltIn
import Interpreter.Util
import Syntax.Expr
import Types.Category

evalExpr :: Env -> Expr -> Value -> IO Value
evalExpr _ expr' VPlaceholder = pure (VExpr expr')
evalExpr env expr' input = 
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
    
    -- NOTE(Maxime): Identity function
    Composition [] -> pure input
    
    -- NOTE(Maxime): a b c $ x -> b c $ a $ x
    Composition (x:xs) -> evalExpr env (Composition xs) =<< evalExpr env x input
 
    Cone values -> VCone <$> sequenceA (Map.map (flip (evalExpr env) input) values)

    -- NOTE(Maxime): can use unsafe due to typecheck
    Cocone mappings -> let 
      VCocone (name, value) = input
      matched               = mappings Map.! name
     in evalExpr env matched value

    ConeProperty prop -> pure (unsafeGet prop input)
    CoconeConstructor name -> pure (VCocone (name, input))
    ConeAnalysis prop -> pure (analyse prop input)

    FunctorApplication functor mappedExpr ->
      case functor of
        {-
        TId -> evalExpr env mappedExpr input
        TUnit -> pure VUnit 
        -- NOTE(Maxime): lookup and replace in functor expression
        TIdentifier name' -> let
            fromType (TypeExpr a) = a
            fromType _            = undefined
            functor'              = FunctorApplication 
              (fromType $ getFunction env name') mappedExpr
          in evalExpr env functor' input
        TArrow _ _ -> undefined
        TFunctor _ _ -> undefined
        -- NOTE(Maxime): {x = a, y = b} F<{x: f, y: g}> -> {x: a F<f>, y: b F<g>}
        TCone typeMap -> let
            VCone vcone = input
            combine v t = evalExpr env (FunctorApplication t mappedExpr) v
            distributed = Map.intersectionWith combine vcone typeMap
           in VCone <$> sequenceA distributed

        TCocone typeMap -> let
            VCocone (prop, val) = input
            functor'            = FunctorApplication (typeMap Map.! prop) mappedExpr
          in VCocone . (prop, ) <$> evalExpr env functor' val
        -}
        Identifier name' -> let
          functor' = FunctorApplication (getFunction env name') mappedExpr
                             in evalExpr env functor' input
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

