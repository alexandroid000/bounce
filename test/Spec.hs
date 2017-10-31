module Main where

import Test.Hspec
import Test.QuickCheck

import Diagrams.Prelude
import BounceSim
import Maps
import GenMapFns



main :: IO ()
main = hspec $ do
    describe "GenMapFns.hs" $ do
        it "Trl Pentagon" $
            (polyLenAngs (mkPoly pent))
            `shouldBe`
            [(499.99999999999994,1.8849555921538759 @@
            rad),(499.99999999999994,1.8849555921538759 @@
            rad),(499.99999999999994,1.884955592153876 @@
            rad),(500.0,1.8849555921538756 @@
            rad),(500.00000000000006,1.8849555921538759 @@ rad)]
