
module Interpreter.PrettyShow (
  pShowValue
                              ) where

import Data.List
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Interpreter.Value
import Syntax.Expr
import Syntax.Type

-- | The enum of all possible colors (ANSI terminal)
data Color
  = Normal
  | CUnit
  | Literal
  | Field
  | Parens
  | Operator
  | Error

-- | ANSI-escapes a color, coloring every subsequent character printed
escape :: Color -> String
escape col = "\ESC[" <> escape' col <> "m"

-- | A map of all ANSI Color-Codes
escape' :: Color -> String
escape' Normal   = "0"
escape' Operator = "0;35"
escape' CUnit    = "0;31"
escape' Literal  = "0;32"
escape' Field    = "4;36"
escape' Parens   = "0;33"
escape' Error    = "1;31"

-- | An operator which colors a certain string, and sets back the terminal color to normal afterwards
(#) :: String -> Color -> String
(#) src col = escape col <> src <> escape Normal


conv :: (t -> String) -> Map.Map Text t -> String
conv f = intercalate ", " 
     . map (\(k, v) -> unpack k #Field <> " = " #Operator <> f v) 
     . Map.toList

pShowValue :: Value -> String
pShowValue VUnit                = "{=}"  #CUnit
pShowValue VPlaceholder         = "%PH%" #Error
pShowValue (VInt n)             = show n #Literal
pShowValue (VFloat x)           = show x #Literal
pShowValue (VExpr x)            = "'" #Operator <> "( " #Parens <> pShowExpr x <> " )" #Parens
pShowValue (VType t)            = pShowType t
pShowValue (VCocone (l, VUnit)) = (unpack l <> ".") #Field
pShowValue (VCocone (l, v))     = pShowValue v <> " " <> (unpack l <> ".") #Field
pShowValue (VCone m)            = "{ " #Parens <> conv pShowValue m <> " }" #Parens


pShowExpr :: Expr -> String
pShowExpr (Composition xs) = unwords . map pShowExpr $ xs
pShowExpr Unit             = "{=}" #CUnit
pShowExpr (Identifier i)   = unpack i #Normal
pShowExpr (FloatLiteral x) = show x #Literal
pShowExpr (IntLiteral n)   = show n #Literal
pShowExpr (UnaryExpression (OtherOp op) e) = unpack op #Operator <> pShowExpr e
pShowExpr (BinaryExpression (OtherOp op) e1 e2) = pShowExpr e1 <> " " <> unpack op #Operator <> " " <> pShowExpr e2
pShowExpr (FunctorApplication t e) = pShowType t <> " < "#Parens <> pShowExpr e <> " >"#Parens
pShowExpr (ConeAnalysis l)      = ("@" <> unpack l) #Field
pShowExpr (ConeProperty l)      = ("." <> unpack l) #Field
pShowExpr (CoconeConstructor l) = (unpack l <> ".") #Field
pShowExpr (TypeExpr t)          = pShowType t
pShowExpr (BuiltIn t)           = unpack t
pShowExpr (Cone m)              = "{ " #Parens <> conv pShowExpr m <> " }" #Parens
pShowExpr (Cocone m)            = "[ " #Parens <> conv pShowExpr m <> " ]" #Parens


pShowType :: Type -> String
pShowType TId   = ""
pShowType TUnit = "{:}" #CUnit
pShowType (TIdentifier i)  = unpack i #Normal
pShowType (TArrow t1 t2)   = pShowType t1 <> " -> " #Operator <> pShowType t2
pShowType (TFunctor t1 t2) = pShowType t1 <> " < "#Parens <> pShowType t2 <> " >"#Parens
pShowType (TCone m)        = "{ " #Parens <> conv pShowType m <> " }" #Parens
pShowType (TCocone m)      = "[ " #Parens <> conv pShowType m <> " ]" #Parens


