module Interpreter.Util (
  unsafeLookup,
  updateUnwrapCocone
                        ) where

import Data.Text
import Interpreter.Value

-- NOTE(Maxime): is only used safely due to typecheck
unsafeLookup :: Eq label => label -> [(label, value)] -> value
unsafeLookup _ []     = undefined
unsafeLookup a (x:xs)
  | fst x == a        = snd x
  | otherwise         = unsafeLookup a xs


updateUnwrapCocone :: Text -> [(Text, Value)] -> [(Text, Value)]
updateUnwrapCocone _ [] = []
updateUnwrapCocone toUnwrap ((name, value):xs)
  | name == toUnwrap = (name, unwrap value) : updateUnwrapCocone toUnwrap xs
  | otherwise        = (name, value)        : updateUnwrapCocone toUnwrap xs
  where
    -- NOTE(Maxime): safe because of typecheck
    unwrap (VCocone (_, inside)) = inside
    unwrap _                     = undefined
