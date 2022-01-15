
module Errors (
  CatrinaError(..),
  mapBoth, unifyTuple
              ) where

import Data.Text
import Interpreter
import Text.Parsec hiding (Error)

mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left x)  = Left (f x)
mapBoth _ f (Right x) = Right (f x)

unifyTuple :: (Either a b, Either a c) -> Either a (b, c)
unifyTuple (Left a, _) = Left a
unifyTuple (_, Left a) = Left a
unifyTuple (Right a, Right b) = Right (a, b)

data CatrinaError
  = ThisIsAGenericError
  | ParserError ParseError
  | IdentifierNotInScope Text
  | ForbiddenConstruction Text

-- NOTE(Maxime): Show "laws" disrespected on purpose for now
instance Show CatrinaError where
  show ThisIsAGenericError = "Generic Error !" #Error
  show (ParserError p)     = show p
  show (IdentifierNotInScope n) = "Identifier "#Error <> unpack n #Field <> " not in scope !"#Error
  show (ForbiddenConstruction n) = "Forbidden construction with " #Error <> unpack n #Field 
