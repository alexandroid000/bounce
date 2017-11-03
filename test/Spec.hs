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
        it "Pts thintriangle" $
            (polyLenAngs (mkPoly thintriangle))
            `shouldBe`
            [(100.00499987500625,3.1215933202164625 @@
            rad),(100.00499987500625,9.999666686665076e-3 @@
            rad),(200.0,9.99966668666552e-3 @@ rad)]
        it "Pts nonconvex" $
            (polyLenAngs (mkPoly four_star))
            `shouldBe`
            [(380.7886552931954,0.8097835725701668 @@
            rad),(380.7886552931954,3.902605407814523 @@
            rad),(380.7886552931954,0.8097835725701668 @@
            rad),(380.7886552931954,3.902605407814523 @@
            rad),(380.7886552931954,0.8097835725701668 @@
            rad),(380.7886552931954,3.902605407814523 @@
            rad),(380.7886552931954,0.8097835725701668 @@
            rad),(380.7886552931954,3.902605407814523 @@ rad)]


