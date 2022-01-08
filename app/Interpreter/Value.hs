module Interpreter.Value (
  Value(..)
                         ) where

import Prelude (Integer, Double, Char, Show, Eq)

import Data.Map
import Data.Text
import Syntax.Type
import Syntax.Expr

data Value
  = VUnit
  | VPlaceholder
  | VInt Integer
  | VFloat Double
  | VShort Char
  | VCone (Map Text Value)
  | VCocone (Text, Value)
  -- NOTE(Maxime): DT here we are
  | VType Type
  | VExpr Expr
  deriving (Eq, Show)


