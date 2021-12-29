module Main where

import Data.Functor
import Data.Text
import Debug.Pretty.Simple
import Interpreter.Program
import Syntax.Program
import System.IO
import Text.Parsec

main :: IO ()
main = openFile "samples" ReadMode
   >>= hGetContents
   <&> parse program "" . pack
   <&> pTraceShowId
   <&> fmap interpretProgram 
   >>= pTraceShowM

-- evaluateExpr :: String -> Either ParseError Value
-- evaluateExpr = fmap (flip (evalExpr []) VUnit) . parse expr "" . pack
