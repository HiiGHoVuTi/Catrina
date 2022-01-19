{-# LANGUAGE RecordWildCards #-}
module Interpreter.Declaration (
  interpretDecl
                               ) where

import Data.Map
import Data.Text
import Interpreter.Util
import Syntax.Declaration
import Syntax.Expr
import Types.Category

interpretDecl :: Env -> Declaration -> Env
interpretDecl env decl = 
  case decl of
    ObjectDeclaration cat name       value -> adjust (addObject      name value) cat env
    ArrowDeclaration  cat name type' value -> adjust (addArrow name type' value) cat env


addObject :: Text -> Expr -> Category -> Category
addObject name value Category{..} =
  Category { objects = insert name value objects, ..}


addArrow :: Text -> Expr -> Expr -> Category -> Category
addArrow name t v Category{..} =
  Category { arrows = insert name (t, v) arrows, .. }
