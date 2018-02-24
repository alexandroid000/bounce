module Test.GenMapFnsSpec (spec) where

import Test.Hspec

import Diagrams.Prelude
import Diagrams.Environments
import Bounce.Simulate
import Bounce.GenMapFns

phi = 5*pi/7 @@ rad
th = 1.2 @@ rad
c = coeff th phi
xfp_correct = 500.0*c/(1+c)

hex_phi = 4*pi/6 @@ rad
hex_c = coeff th hex_phi
hex_correct = 500.0*hex_c/(1+hex_c)


-- rounding for floating point checks
rc :: Double -> Double
rc f = (fromIntegral $ round (10^5 * f))/(10^5)

spec :: Spec
spec = do
    describe "GenMapFns" $ do
        it "fp odd sides" $
            rc (xfp (mkPoly hep) (1.2 @@ rad))
            `shouldBe`
            rc xfp_correct
        it "fp even sides" $
            rc (xfp (mkPoly hex) (1.2 @@ rad))
            `shouldBe`
            rc hex_correct
