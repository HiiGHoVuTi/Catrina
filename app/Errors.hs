{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleInstances #-}

module Errors (
  CatrinaError(..),
  mapBoth, unifyTuple
              ) where

import Control.DeepSeq
import Data.Text
import GHC.Generics (Generic)
import Interpreter

mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left x)  = Left (f x)
mapBoth _ f (Right x) = Right (f x)

unifyTuple :: (Either a b, Either a c) -> Either a (b, c)
unifyTuple (Left a, _) = Left a
unifyTuple (_, Left a) = Left a
unifyTuple (Right a, Right b) = Right (a, b)

data CatrinaError
  = ThisIsAGenericError
  | ParserError Text
  | IdentifierNotInScope Text
  | ForbiddenConstruction Text
  | TypeNotMatching
  | TypeNotFound
  deriving (Generic)

instance NFData CatrinaError


instance MonadFail (Either CatrinaError) where
  fail = const $ Left TypeNotFound

-- NOTE(Maxime): Show "laws" disrespected on purpose for now
instance Show CatrinaError where
  show ThisIsAGenericError = "Generic Error !" #Error
  show (ParserError p)     = unpack p
  show (IdentifierNotInScope n) = "Identifier "#Error <> unpack n #Field <> " not in scope !"#Error
  show (ForbiddenConstruction n) = "Forbidden construction with " #Error <> unpack n #Field 
  show TypeNotMatching = "Type not matching -- a better error is on its way"
  show TypeNotFound = "Type not found -- a better error is on its way"
