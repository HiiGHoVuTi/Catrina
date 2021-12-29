module Types.Category (
  Category(..)
                      ) where

import Data.Map
import Data.Text
import Syntax.Expr
import Syntax.Type

type Arrow = (Type, Expr)

data Category = Category
  { objects  :: Map Text Type
  , arrows   :: Map Text Arrow
  , compose  :: Arrow
  -- NOTE(Maxime): ComposeCone and ComposeCocone ?
  }
  deriving Show
