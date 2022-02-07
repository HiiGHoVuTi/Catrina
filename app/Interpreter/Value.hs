{-# LANGUAGE DeriveGeneric #-}
module Interpreter.Value (
  Value(..)
                         ) where

import Prelude (Integer, Double, Char, Show, Eq)

import Control.DeepSeq
import Data.Map
import Data.Text
import GHC.Generics (Generic)
import Syntax.Expr

data Value
  = VUnit
  | VPlaceholder
  | VInt Integer
  | VFloat Double
  | VShort Char
  | VCone (Map Text Value)
  | VCocone (Text, Value)
  | VExpr Expr
  deriving (Eq, Show, Generic)

instance NFData Value
