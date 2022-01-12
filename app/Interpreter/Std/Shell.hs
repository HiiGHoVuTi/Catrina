{-# LANGUAGE OverloadedStrings #-}
module Interpreter.Std.Shell (
  shell, stdout
                             ) where

import qualified Data.Map as Map
import Interpreter.Value
import System.Process hiding (shell)

import Debug.Pretty.Simple

shell :: Value -> IO Value
shell (VCone m) = unstringify <$> readProcess 
      (stringify (m Map.! "path"))
      (map stringify . init . listify $ m Map.! "args")
      (stringify (m Map.! "stdin"))
shell _ = undefined

stdout :: Value -> IO Value
stdout x = do
  putStrLn $ stringify x
  pure VUnit

stringify :: Value -> String
stringify (VCone m') = 
    (\(VShort c) -> c) (m' Map.! "head") 
    : stringify        (m' Map.! "tail")
stringify (VCocone ("cons", VCone m')) = stringify (VCone m')
stringify (VCocone ("empty", _)) = ""
stringify a          = pTraceShow a ""

unstringify :: String -> Value
unstringify (x:xs) =
  VCocone ("cons"
  , VCone $ Map.fromList
    [ ("head", VShort x)
    , ("tail", unstringify xs)
    ]
  )
unstringify []     = VCocone ("empty", VUnit)

listify :: Value -> [Value]
listify (VCone m') = m' Map.! "head" : listify (m' Map.! "tail")
listify (VCocone ("cons", a)) = listify a
listify x          = pure x

