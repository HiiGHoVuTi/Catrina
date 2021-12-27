module Main where

import Data.Functor
import Data.Text
import Debug.Pretty.Simple
import Interpreter.Expr
import Interpreter.Value
import Syntax.Expr
import Syntax.Type
import System.IO
import Text.Parsec

main :: IO ()
main = openFile "samples" ReadMode
   >>= hGetContents
   <&> parse expr "" . pack
   -- <&> fmap (flip (evalExpr []) VUnit)
   >>= pTraceShowM

evaluateExpr :: String -> Either ParseError Value
evaluateExpr = fmap (flip (evalExpr []) VUnit) . parse expr "" . pack
