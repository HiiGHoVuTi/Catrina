module Syntax.Common (
  pair, tuple
                     ) where

import qualified Data.Map as Map
import Data.Text hiding (zip, reverse)
import Parsing
import Text.Parsec.Token

pair :: String -> Parser a -> Parser (Text, a)
pair op parser = do
  name <- pack <$> identifier lexer
  reservedOp lexer op
  value <- parser
  pure(name, value)

-- NOTE(Maxime): maybe replace _1, _2 by 1, 2
tuple :: [a] -> Map.Map Text a
tuple = Map.fromList . zip [(pack . ("_" <>) . show) n | n <- [1 :: Integer ..]]


