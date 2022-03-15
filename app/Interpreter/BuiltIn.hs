{-# LANGUAGE OverloadedStrings #-}

module Interpreter.BuiltIn (
  Value(..), executeStd
                           ) where
import qualified Data.Text as T
import Interpreter.PrettyShow
import Interpreter.Std
import Interpreter.Value

executeStd :: T.Text -> Value -> IO Value
-- Prelude
executeStd "id" = pure
executeStd "shell"  = shell
executeStd "stdout" = stdout
executeStd "show_"  = pure.unstringify.pShowValue
executeStd "show"   = pure.unstringify.showValue

executeStd "==" = pure.eq
executeStd "!=" = pure.neq

-- NumericPrelude
executeStd "+"  = pure.add
executeStd "-"  = pure.sub
executeStd "*"  = pure.mult
executeStd "/"  = pure.div'

executeStd ">!=" = pure.gt
executeStd "<!=" = pure.lt
executeStd ">="  = pure.geq
executeStd "<="  = pure.leq

-- Recursion schemes
executeStd "const" = pure.VExpr . revertToExpr
executeStd "cata"  = pure.undefined

-- Rest
executeStd name = const $ error 
                        $ T.unpack ("Can't find " `T.append` name)

