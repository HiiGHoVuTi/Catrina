{-# LANGUAGE OverloadedStrings #-}

module Interpreter.BuiltIn (
  Value(..), executeStd
                           ) where
import qualified Data.Text as T
import Interpreter.Std
import Interpreter.Value

executeStd :: T.Text -> Value -> IO Value
-- Prelude
executeStd "id" = pure
executeStd "shell"  = shell
executeStd "stdout" = stdout

executeStd "==" = pure.eq
executeStd "/=" = pure.neq

-- NumericPrelude
executeStd "+"  = pure.add
executeStd "-"  = pure.sub
executeStd "*"  = pure.mult
executeStd "/"  = pure.div'

executeStd ">"  = pure.gt
executeStd "<"  = pure.lt
executeStd ">=" = pure.geq
executeStd "<=" = pure.leq

-- Rest
-- FIXME(Maxime): actually use @@single c *> identifier@@
executeStd name = const $ error 
                        $ T.unpack ("Can't find " `T.append` name)

