module Parsing (

               ) where

import Data.Text
import Data.Void
import Text.Parsec


type Parser = Parsec Void Text


