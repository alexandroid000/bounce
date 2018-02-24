module Main where
        
import Test.Hspec.Runner
import Test.Hspec.Formatters
--import Test.QuickCheck
--import Test.SpaceExTests
--import Test.SimTests

import qualified Spec

main :: IO ()
main = hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
