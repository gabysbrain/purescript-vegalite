module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
--import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Reporter.Xunit (xunitReporter)
import Test.Spec.Runner (RunnerEffects, run)

type TestEffects = RunnerEffects (fs::FS, exception::EXCEPTION)
--type TestEffects = RunnerEffects (fs::FS, exception::EXCEPTION, random::RANDOM)

main :: Eff TestEffects Unit
main = discover "Test\\..*Spec" 
  >>= run 
    [ consoleReporter
    , xunitReporter { indentation: 2, outputPath: "output/test.xml" }
    ]

