module Test.SchemaValidate.Validator where

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Argonaut.Core (Json)

foreign import validateImpl :: Fn2 Json Json Boolean

validate :: Json -> Json -> Boolean
validate s v = runFn2 validateImpl s v

