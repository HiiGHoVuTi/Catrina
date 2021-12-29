{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Std.Numbers (
  add, sub, mult
                               ) where

import Data.Map
import Interpreter.Value

add :: Value -> Value
add (VCone m) = f (m ! "_1") (m ! "_2")
  where
    f (VInt   _1) (VInt _2)   = VInt   (_1 + _2)
    f (VFloat _1) (VFloat _2) = VFloat (_1 + _2)
    f _           _           = undefined
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
