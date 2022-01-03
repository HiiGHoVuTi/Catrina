{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Std.Boolean (
  eq, neq
                               ) where

import Data.Map
import Interpreter.Value

boolToCocone :: Bool -> Value
boolToCocone True  = VCocone ("true" , VUnit)
boolToCocone False = VCocone ("false", VUnit)

eq :: Value -> Value
eq (VCone m) = boolToCocone $ (m ! "_1") == (m ! "_2")
eq _         = undefined

neq :: Value -> Value
neq (VCone m) = boolToCocone $ (m ! "_1") /= (m ! "_2")
neq _         = undefined
