{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Std.Numbers (
  add, sub, mult, div',
  lt, gt, leq, geq
                               ) where

import Data.Map
import Interpreter.Value

import Debug.Pretty.Simple

-- NOTE(Maxime): cannot un-duplicate the code due to type checking

add :: Value -> Value
add (VCone m) = f (m ! "_1") (m ! "_2")
  where
    f (VInt   _1) (VInt _2)   = VInt   (_1 + _2)
    f (VFloat _1) (VFloat _2) = VFloat (_1 + _2)
    f a           b           = pTraceShow (a,b) undefined
-- NOTE(Maxime): When concepts/typeclasses are introduced, add any num
add _ = undefined
  
sub :: Value -> Value
sub (VCone m) = f (m ! "_1") (m ! "_2")
  where
    f (VInt   _1) (VInt _2)   = VInt   (_1 - _2)
    f (VFloat _1) (VFloat _2) = VFloat (_1 - _2)
    f _           _           = undefined
-- NOTE(Maxime): When concepts/typeclasses are introduced, add any num
sub _ = undefined

mult :: Value -> Value
mult (VCone m) = f (m ! "_1") (m ! "_2")
  where
    f (VInt   _1) (VInt _2)   = VInt   (_1 * _2)
    f (VFloat _1) (VFloat _2) = VFloat (_1 * _2)
    f _           _           = undefined
-- NOTE(Maxime): When concepts/typeclasses are introduced, add any num
mult _ = undefined

div' :: Value -> Value
div' (VCone m) = f (m ! "_1") (m ! "_2")
  where
    f (VInt   _1) (VInt _2)   = VInt   (_1 `div` _2)
    f (VFloat _1) (VFloat _2) = VFloat (_1 / _2)
    f _           _           = undefined
-- NOTE(Maxime): When concepts/typeclasses are introduced, add any num
div' _ = undefined



gt :: Value -> Value
gt (VCone m) = f (m ! "_1") (m ! "_2")
  where
    f (VInt   _1) (VInt _2)   = toBool (_1 > _2)
    f (VFloat _1) (VFloat _2) = toBool (_1 > _2)
    f _           _           = undefined
-- NOTE(Maxime): When concepts/typeclasses are introduced, add any num
gt _ = undefined

lt :: Value -> Value
lt (VCone m) = f (m ! "_1") (m ! "_2")
  where
    f (VInt   _1) (VInt _2)   = toBool (_1 < _2)
    f (VFloat _1) (VFloat _2) = toBool (_1 < _2)
    f _           _           = undefined
-- NOTE(Maxime): When concepts/typeclasses are introduced, add any num
lt _ = undefined

geq :: Value -> Value
geq (VCone m) = f (m ! "_1") (m ! "_2")
  where
    f (VInt   _1) (VInt _2)   = toBool (_1 >= _2)
    f (VFloat _1) (VFloat _2) = toBool (_1 >= _2)
    f _           _           = undefined
-- NOTE(Maxime): When concepts/typeclasses are introduced, add any num
geq _ = undefined

leq :: Value -> Value
leq (VCone m) = f (m ! "_1") (m ! "_2")
  where
    f (VInt   _1) (VInt _2)   = toBool (_1 <= _2)
    f (VFloat _1) (VFloat _2) = toBool (_1 <= _2)
    f _           _           = undefined
-- NOTE(Maxime): When concepts/typeclasses are introduced, add any num
leq _ = undefined

toBool :: Bool -> Value
toBool True  = VCocone ("true" , VUnit)
toBool False = VCocone ("false", VUnit)
