{-# LANGUAGE OverloadedStrings #-}
module Interpreter.Std.Shell (
  shell
                             ) where

import qualified Data.Map as Map
import Interpreter.Value
import System.Process hiding (shell)

import Debug.Pretty.Simple

shell :: Value -> IO Value
shell (VCone m) = unstringify <$> readProcess 
      (stringify (m Map.! "path"))
      (map stringify . listify $ m Map.! "args")
      (stringify (m Map.! "stdin"))
  where
    stringify :: Value -> String
    stringify (VCone m') = undefined
    stringify (VCocone ("cons", VCone m')) = stringify (VCone m')
    stringify (VCocone ("empty", _)) = ""
    stringify a          = pTraceShow a ""
    
    unstringify :: String -> Value
    unstringify (x:xs) = VCocone 
      ("cons"
      , VCone $ Map.fromList
        [ ("head", VShort x)
        , ("tail", unstringify xs)
        ]
      )
    unstringify []     = VCocone ("empty", VUnit)

    listify :: Value -> [Value]
    listify (VCone m') = m' Map.! "head" : listify (m' Map.! "tail")
    listify x          = pure x

shell _ = undefined
