
{-# LANGUAGE OverloadedStrings #-}

module Interpreter.PrettyShow (
  showValue, pShowValue, (#), Color(..)
                              ) where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text, unpack, intercalate)
import Interpreter.Value
import Syntax.Expr

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


conv :: (t -> String) -> String -> Map.Map Text t -> String
conv f eq = List.intercalate ", " 
     . map (\(k, v) -> unpack k #Field <> eq#Operator <> f v) 
     . Map.toList

isChr :: Value -> Bool
isChr (VShort _) = True
isChr _          = False

renderString :: Map.Map Text Value -> String
renderString m' = 
      (\(VShort c) -> c) (m' Map.! "head") 
      : case m' Map.! "tail" of
          VCocone ("cons", VCone m'') -> renderString m'' 
          _                           -> ""

renderList :: Map.Map Text Value -> String
renderList m' = 
      pShowValue (m' Map.! "head") 
      <> case m' Map.! "tail" of
          VCocone ("cons", VCone m'') -> ", "#Operator <> renderList m'' 
          _                           -> ""


pShowValue :: Value -> String
pShowValue VUnit                = "{:}"  #CUnit
pShowValue VPlaceholder         = "Runtime Panic !" #Error
pShowValue (VInt n)             = show n #Literal
pShowValue (VFloat x)           = show x #Literal
pShowValue (VShort c)           = show c #Literal
pShowValue (VExpr x)            = "'" #Operator <> "( " #Parens <> pShowExpr x <> " )" #Parens
pShowValue (VCocone (l, VUnit)) = (unpack l <> ".") #Field
pShowValue ( VCocone ("cons"
           , VCone m ))
           | isChr (m Map.! "head")   = ("\"" <> renderString m <> "\"") #Literal
           | otherwise                = "#("#Parens <> renderList m <> ")"#Parens
pShowValue (VCocone (l, v))     = pShowValue v <> " " <> (unpack l <> ".") #Field
pShowValue (VCone m)            = "{ " #Parens <> conv pShowValue " : " m <> " }" #Parens
 
pShowExpr :: Expr -> String
pShowExpr (Composition xs)  = unwords . map pShowExpr $ xs
pShowExpr Unit              = "{:}" #CUnit
pShowExpr (Identifier i)    = unpack i #Normal
pShowExpr (FloatLiteral x)  = show x #Literal
pShowExpr (IntLiteral n)    = show n #Literal
pShowExpr (StringLiteral s) = show s #Literal
pShowExpr (CharLiteral c)   = show c #Literal
pShowExpr (UnaryExpression (OtherOp op) e) = unpack op #Operator <> pShowExpr e
pShowExpr (BinaryExpression (OtherOp op) e1 e2) = "("#Parens <> pShowExpr e1 <> " " <> unpack op #Operator <> " " <> pShowExpr e2 <> ")"#Parens
pShowExpr (FunctorApplication t e) = pShowExpr t <> " < "#Parens <> pShowExpr e <> " >"#Parens
pShowExpr (ConeAnalysis ls)     = ("@" <> unpack (intercalate ";" ls)) #Field
pShowExpr (ConeProperty l)      = ("." <> unpack l) #Field
pShowExpr (CoconeConstructor l) = (unpack l <> ".") #Field
pShowExpr (BuiltIn t)           = unpack t
pShowExpr (Cone m)              = "{ " #Parens <> conv pShowExpr " : " m <> " }" #Parens
pShowExpr (Cocone m)            = "[ " #Parens <> conv pShowExpr " : " m <> " ]" #Parens

showValue :: Value -> String
showValue (VInt   i) = show i
showValue (VFloat x) = show x
showValue ( VCocone ("cons"
           , VCone m ))
          | isChr (m Map.! "head") = renderString m
showValue _ = error "WIP"
