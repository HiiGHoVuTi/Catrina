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
  , "String", "Int", "Float", "Char"
  -- Built-Ins
  , "shell", "stdout", "id"
  ]

getIdentifiers :: Program -> [Text]
getIdentifiers (Program (ArrowDeclaration _ n _ _:xs)) = n : getIdentifiers (Program xs)
getIdentifiers (Program (ObjectDeclaration _ n _ :xs)) = n : getIdentifiers (Program xs)
getIdentifiers (Program []) = []
