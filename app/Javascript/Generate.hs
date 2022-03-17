{-# LANGUAGE OverloadedStrings #-}

module Javascript.Generate (
  generateJs
                           ) where

import Data.Functor.Foldable
import qualified Data.Map as Map
import qualified Data.Text as T
import Syntax.Declaration
import Syntax.Expr
import Syntax.Program

import Debug.Pretty.Simple

generateJs :: Program -> String
generateJs Program 
  { programHeader = h
  , imports = i
  , programDeclarations = d
  } = pTraceShowId $ 
    foldWithDefault ((<>) . (<> "\n")) ""
      [ "import '" <> T.unpack name <> "'" | name <- h]
    <> "\n\n"
    <> foldWithDefault ((<>) . (<> "\n")) ""
      (map generateDecl d)
    <> "\n\n"
    <> ( "module.exports = { "
      <> foldWithDefault ((<>) . (<> ", ")) "" (map T.unpack i)
      <> " }"
       )

generateDecl :: Declaration -> String
generateDecl ObjectDeclaration{} = ""
generateDecl (ArrowDeclaration _ _ _ (Identifier "EXTERNAL")) = ""
generateDecl (ArrowDeclaration "Base" name _ body) 
  = "function "<> T.unpack name <>"(x) {\n"
  <> indent (generateExpr body) <> "\n"
  <> "  return x;\n"
  <> "}\n"
generateDecl _ = ""

generateExpr :: Expr -> String
generateExpr = para go
  where
    go :: ExprF (Expr, String) -> String
    go UnitF = "x = {};\n"
    go (CompositionF [ (Composition [
      BinaryExpression (OtherOp ":,") 
        (StringLiteral t) 
        _, _ ], _), (Identifier "external", _)]) 
      = "x = " <> t <> "(x);\n"
    go (CompositionF xs) = foldl (<>) "" (map snd xs)
    go (IdentifierF "id") = ""
    go (IdentifierF "const") = "x = (a => b => a)(x);\n"
    go (IdentifierF t) = "x = " <> T.unpack t <> "(x);\n"
    go (CharLiteralF c) = "x = " <> pure c <> ";\n"
    go (IntLiteralF i) = "x = " <> show i <> ";\n"
    go (FloatLiteralF x) = "x = " <> show x <> ";\n"
    -- FIXME(Maxime): strings are totally broken
    go (StringLiteralF s) = "x = \"" <> s <> "\";\n"
    go (UnaryExpressionF (OtherOp "`") r)
      =  "x = (function (x){\n"
      <> indent (snd r) 
      <> "  return x;\n})(y)(x);\n"
    go (UnaryExpressionF (OtherOp "'") r) 
      =  "x = (function (y){\n" <> indent
        (  "x = x => {\n" 
        <> indent (snd r)
        <> "  return x;\n};\n"
        )
      <> "  return x;\n})(x);\n"
    go (UnaryExpressionF (OtherOp o) r) = snd r <> "x = " <> T.unpack o <> "x;\n"
    go (BinaryExpressionF (OtherOp o) a b) 
      =  "function lhs(x){\n" <> indent (snd a) <> "  return x;\n}\n"
      <> "function rhs(x){\n" <> indent (snd b) <> "  return x;\n}\n"
      <> finishBinop (T.unpack o)
    go (ConeF m) = "x = {\n" <> indent
        ( concatMap snd $ Map.toList $ flip Map.mapWithKey m $ \t s ->
          T.unpack t <> ": (function(x){\n" <> indent (snd s) <> "  return x;\n})(x),\n"
        )
      <> "};\n"
    go (ConePropertyF t) = "x = x." <> T.unpack t <> ";\n"
    go (CoconeF m) = "x = (function ([a, x]) {\n" <> indent 
      ( concatMap snd $ Map.toList $ flip Map.mapWithKey m $ \t s ->
        "if (a == \"" <> T.unpack t <> "\") {\n" <> indent (snd s) <> "}\n"
      )
      <> "  return x;\n})(x);\n"
    go (CoconeConstructorF t) = "x = [\"" <> T.unpack t <> "\", x];\n"
    go (ConeAnalysisF ts) = undefined
    --FIXME
    go (FunctorApplicationF _ _) = undefined
    go (BuiltInF _) = undefined
    --go _ = ""

finishBinop :: String -> String
finishBinop "<!=" = "x = lhs(x) < rhs(x);\n"
finishBinop ">!=" = "x = lhs(x) > rhs(x);\n"
finishBinop "==" = "x = lhs(x) === rhs(x);\n"
finishBinop "$" = "x = lhs(x)(rhs(x));\n"
finishBinop ">>>" = "x = rhs(x)(lhs(x));\n"
finishBinop ":," = "x = [\"cons\", { head: lhs(x), tail: rhs(x) }];\n"
finishBinop o = "x = lhs(x) " <> o <> " rhs(x);\n"

foldWithDefault :: (a -> a -> a) -> a -> [a] -> a
foldWithDefault _ d [ ] = d
foldWithDefault _ _ [x] = x
foldWithDefault f _ xs  = foldl1 f xs

indent :: String -> String
indent = unlines . map ("  " <>) . lines
