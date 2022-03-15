{-# LANGUAGE DeriveGeneric, TemplateHaskell, TypeFamilies, DeriveFunctor, DeriveTraversable #-}
module Interpreter.Value (
  Value(..), revertToExpr
                         ) where

import Control.DeepSeq
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Map
import Data.Text
import GHC.Generics (Generic)
import Syntax.Expr

data Value
  = VUnit
  | VPlaceholder
  | VInt Integer
  | VFloat Double
  | VShort Char
  | VCone (Map Text Value)
  | VCocone (Text, Value)
  | VExpr Expr
  deriving (Eq, Show, Generic)

makeBaseFunctor ''Value

instance NFData Value

revertToExpr :: Value -> Expr
revertToExpr = cata go
  where
    go (VExprF e) = e
    go VUnitF = Unit
    go (VIntF i) = IntLiteral i
    go (VFloatF f) = FloatLiteral f
    go (VShortF c) = CharLiteral c
    go (VConeF m) = Cone m
    go (VCoconeF (t, m)) = Composition [m, CoconeConstructor t]
    go VPlaceholderF = error ""
