module Test.Util where

import Prelude
import Control.Monad.Aff (Aff())
import Data.Array as A
import Data.Foldable (and)
import Test.Spec.Assertions (fail)
--import Test.QuickCheck (Result, (<?>))

-- aproximate equality
class ApproxEq a where
  approxEq :: Number -> a -> a -> Boolean

--newtype ApproxNum = ApproxNum Number

instance approxEqNumber :: ApproxEq Number where
  approxEq eps x y = (y - x) <= eps && (y - x) >= (-eps)

instance approxEqArray :: ApproxEq a => ApproxEq (Array a) where
  approxEq eps x y = and $ A.zipWith (approxEq eps) x y

{--assertApproxEquals :: forall a. ApproxEq a => Show a => Number -> a -> a -> Result--}
{--assertApproxEquals eps a b = approxEq eps a b --}
                          {--<?> show a <> " ≇ " <> show b --}
                           {--<> " (ℇ = " <> show eps <> ")"--}

shouldApproxEq :: forall r t. Show t => ApproxEq t => Number -> t -> t -> Aff r Unit
shouldApproxEq eps v1 v2 =
  when (not (approxEq eps v1 v2)) $
    fail $ show v1 <> " ≇ " <> show v2 <> " (ℇ = " <> show eps <> ")"


