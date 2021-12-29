module Interpreter.Value (
  Value(..)
                         ) where

import Prelude (Integer, Double, Show)

import Data.Map
import Data.Text
import Syntax.Type

data Value
  = VUnit
  | VInt Integer
  | VFloat Double
  | VCone (Map Text Value)
  | VCocone (Text, Value)
  -- NOTE(Maxime): DT here we are
  | VType Type
  deriving (Show)


