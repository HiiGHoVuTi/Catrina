module Types.Category (
  Category(..)
                      ) where

import Data.Text
import Syntax.Expr
import Syntax.Type

type Arrow = (Type, Expr)

data Category = Category
  { objects  :: [(Text, Type)]
  , arrows   :: [(Text, Arrow)]
  , compose  :: Arrow
  -- NOTE(Maxime): ComposeCone and ComposeCocone ?
  }
