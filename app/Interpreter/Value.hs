module Interpreter.Value (
  Value(..)
                         ) where

import Prelude (Integer, Double, Char, Show, Eq)

import Data.Map
import Data.Text
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
  deriving (Eq, Show)


