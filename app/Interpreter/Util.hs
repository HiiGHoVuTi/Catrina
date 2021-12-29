module Interpreter.Util (
  Env,
  updateUnwrapCocone
                        ) where

import Data.Map
import Data.Text hiding (empty)
import Interpreter.Value
import Types.Category

type Env = Map Text Category

updateUnwrapCocone :: Text -> Map Text Value -> Map Text Value
updateUnwrapCocone = adjust unwrap 
  where
    -- NOTE(Maxime): safe because of typecheck
    unwrap (VCocone (_, inside)) = inside
    unwrap _                     = undefined
