{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Std.Numbers (
  add, sub, mult
                               ) where

-- NOTE(Maxime): _1 / _2 vs lhs/rhs

import Interpreter.Value

add :: Value -> Value
add (VCone 
  [ ("_1", VInt _1)
  , ("_2", VInt _2)
  ]) = VInt (_1 + _2)
add (VCone 
  [ ("_1", VFloat _1)
  , ("_2", VFloat _2)
  ]) = VFloat (_1 + _2)
-- NOTE(Maxime): When concepts/typeclasses are introduced, add any num
add _ = undefined

sub :: Value -> Value
sub (VCone 
  [ ("_1", VInt _1)
  , ("_2", VInt _2)
  ]) = VInt (_1 - _2)
sub (VCone 
  [ ("_1", VFloat _1)
  , ("_2", VFloat _2)
  ]) = VFloat (_1 - _2)
-- NOTE(Maxime): When concepts/typeclasses are introduced, add any num
sub _ = undefined

mult :: Value -> Value
mult (VCone 
  [ ("_1", VInt _1)
  , ("_2", VInt _2)
  ]) = VInt (_1 * _2)
mult (VCone 
  [ ("_1", VFloat _1)
  , ("_2", VFloat _2)
  ]) = VFloat (_1 * _2)
-- NOTE(Maxime): When concepts/typeclasses are introduced, add any num
mult _ = undefined
