module Main where

import Data.Text
import Debug.Pretty.Simple
import Syntax.Program
import System.IO
import Text.Parsec

main :: IO ()
main = openFile "samples" ReadMode
   >>= hGetContents
   >>= pTraceShowM . parse program "" . pack

   
