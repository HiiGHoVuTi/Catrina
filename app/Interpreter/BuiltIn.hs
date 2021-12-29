{-# LANGUAGE OverloadedStrings #-}

module Interpreter.BuiltIn (
  Value(..), executeStd
                           ) where
import qualified Data.Map as Map
import qualified Data.Text as T
import Interpreter.Std
import Interpreter.Util
import Interpreter.Value

executeStd :: T.Text -> Value -> Value
-- Prelude
executeStd "id" = id

-- NumericPrelude
executeStd "+" = add
executeStd "-" = sub
executeStd "*" = mult

-- Rest
executeStd name
  | "." `T.isPrefixOf` name = unsafeGet   (T.tail name)
  | "." `T.isSuffixOf` name = makeVCocone (T.init name)
  | "@" `T.isPrefixOf` name = analyse     (T.tail name)
  | otherwise               = const $ error 
                            $ T.unpack ("Can't find " `T.append` name)


-- NOTE(Maxime): error is mostly for debugging purposes
-- as those would be caught by the typechecker
unsafeGet :: T.Text -> Value -> Value
unsafeGet v (VCone a) = a Map.! v
unsafeGet name value  = error 
                      $ "Attempting to use ." 
                      <> T.unpack name 
                      <> " on non-cone: \n" 
                      <> show value

-- NOTE(Maxime): Yes the pointfree is useless ;)
makeVCocone :: T.Text -> Value -> Value
makeVCocone = (VCocone .) . (,)

-- FIXME(Maxime): refactor
analyse :: T.Text -> Value -> Value
analyse name (VCone m) = let
  (VCocone (field, _)) = m Map.! name
  newmap               = updateUnwrapCocone name m
  in VCocone (field, VCone newmap)
analyse name value     = error 
                       $ "Attempting to use @" 
                       <> T.unpack name 
                       <> " on non-cone: \n" 
                       <> show value

