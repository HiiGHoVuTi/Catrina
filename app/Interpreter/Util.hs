module Interpreter.Util (
  unsafeLookup
                        ) where

-- NOTE(Maxime): is only used safely due to typecheck
unsafeLookup :: Eq label => label -> [(label, value)] -> value
unsafeLookup _ []     = undefined
unsafeLookup a (x:xs)
  | fst x == a        = snd x
  | otherwise         = unsafeLookup a xs

