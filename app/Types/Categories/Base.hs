{-# LANGUAGE OverloadedStrings #-}

module Types.Categories.Base (
  base, catO'Cats
                             ) where

import Data.Map
import Syntax.Expr
import Types.Category
import qualified Types.Category as Category

base :: Category 
base = Category
  { objects = empty
  , arrows  = empty
  -- NOTE(Maxime): Base can't be specified in the language
  , Category.compose = (Unit, Unit)
  }


catO'Cats :: Category
catO'Cats = Category
  { objects = fromList 
    [ ("Cat" , Identifier "Cat")
    , ("Base", Identifier "Base")
    ]
  , arrows  = empty -- NOTE(Maxime): add some built-ins
  , Category.compose = (Unit, Unit)
  }
