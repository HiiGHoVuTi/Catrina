module Types.Category (
  Category(..)
                      ) where

import Data.Map
import Data.Text
import Syntax.Expr

type Arrow = (Expr, Expr)

data Category = Category
  { objects  :: Map Text Expr
  , arrows   :: Map Text Arrow
  , compose  :: Arrow
  -- NOTE(Maxime): ComposeCone and ComposeCocone ?
  }
  deriving Show
