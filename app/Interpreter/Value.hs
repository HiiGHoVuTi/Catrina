module Interpreter.Value (
  Value(..)
                         ) where

import Data.Text
import Syntax.Type

data Value
  = VUnit
  | VInt Integer
  | VFloat Double
  | VCone [(Text, Value)]
  | VCocone (Text, Value)
  -- NOTE(Maxime): DT here we are
  | VType Type
  deriving (Show)


