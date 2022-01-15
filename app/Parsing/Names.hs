module Parsing.Names (
  reservedNames', reservedOpNames'
                     ) where

reservedNames', reservedOpNames' :: [String]
reservedNames'   = ["ar", "ob", "import", "export"]
reservedOpNames' = [".", "@", "->"]
