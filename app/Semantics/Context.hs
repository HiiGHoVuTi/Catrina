{-# LANGUAGE OverloadedStrings #-}
module Semantics.Context (
  Context(..), createContext
                         ) where

import Data.Text
import Syntax.Declaration
import Syntax.Program

data Context = Context
  { existingIdentifiers :: [Text]
  }

createContext :: Program -> Context
createContext p = Context
  { existingIdentifiers = baseIdentifiers ++ getIdentifiers p
  }

baseIdentifiers :: [Text]
baseIdentifiers =
  [ "Base", "Cat", "Fun", "Shell"
  -- Types
  , "String", "Int", "Float", "Char", "Bool"
  -- Built-Ins
  , "id", "cons", "fmap", "cata"
  , "shell", "stdout", "show", "strconcat"
  ]

-- FIXME(Maxime): inputs
getIdentifiers :: Program -> [Text]
getIdentifiers (Program _ _ (ArrowDeclaration _ n _ _:xs)) = n : getIdentifiers (Program [][]xs)
getIdentifiers (Program _ _ (ObjectDeclaration _ n _ :xs)) = n : getIdentifiers (Program [][]xs)
getIdentifiers (Program _ _ []) = []
