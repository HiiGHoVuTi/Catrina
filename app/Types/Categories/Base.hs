{-# LANGUAGE OverloadedStrings #-}

module Types.Categories.Base (
  base
                             ) where

import Data.Map
import Syntax.Expr
import Syntax.Type
import Types.Category

base :: Category 
base = Category
  { objects = empty
  , arrows  = empty
  -- NOTE(Maxime): Base can't be specified in the language
  , compose = (TUnit, Unit)
  }

