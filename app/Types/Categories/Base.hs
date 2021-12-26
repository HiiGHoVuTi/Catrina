{-# LANGUAGE OverloadedStrings #-}

module Types.Categories.Base (
  base
                             ) where

-- NOTE(Maxime): Might have to actually hardcode the stuff because yes.

-- import Syntax.Expr
import Syntax.Type
import Types.Category

base :: Category 
base = Category
  { objects = [("Int", TUnit)]
  , arrows  = []
  , compose = (undefined, undefined)
  }

