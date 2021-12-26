module Interpreter.Value (
  Value(..)
                         ) where

import Data.Text

-- NOTE(Maxime): Sum and Product types
data Value
  = VUnit
  | VInt Integer
  | VFloat Double
  | VCone [(Text, Value)]
  deriving (Show)


