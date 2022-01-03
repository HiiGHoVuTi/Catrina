{-# LANGUAGE OverloadedStrings #-}

module Interpreter.BuiltIn (
  Value(..), executeStd
                           ) where
import qualified Data.Text as T
import Interpreter.Std
import Interpreter.Value

executeStd :: T.Text -> Value -> Value
-- Prelude
executeStd "id" = id

executeStd "==" = eq
executeStd "/=" = neq

-- NumericPrelude
executeStd "+" = add
executeStd "-" = sub
executeStd "*" = mult

-- Rest
-- FIXME(Maxime): actually use @@single c *> identifier@@
executeStd name = const $ error 
                        $ T.unpack ("Can't find " `T.append` name)

