module Main where

import Test.Hspec
import Test.QuickCheck

import Diagrams.Prelude
import BounceSim
import Maps
import GenMapFns
import HA
import GenHA

phi = 5*pi/7 @@ rad
th = 1.2 @@ rad
c = coeff th phi
xfp_correct = 500.0*c/(1+c)

hex_phi = 4*pi/6 @@ rad
hex_c = coeff th hex_phi
hex_correct = 500.0*hex_c/(1+hex_c)

loc1 = Location 1 "interior" "-500.0 &lt;= x &amp;&amp; x &lt;= 0.0 &amp;&amp; 0 &lt;= y &amp;&amp; y &lt;= 500" "x'==vx &amp; y'==vy"

square_ha :: HA
square_ha = HA   { name = "test"
               , params = mkParams $ mkPoly sq
               , locations = [loc1]
               , transitions = mkTs (mkPoly sq) ((pi/2)-0.05 @@ rad)
               }


main :: IO ()
main = hspec $ do
    describe "Bouncing Tests" $ do
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
        it "fp odd sides" $
            (xfp (mkPoly hep) (1.2 @@ rad))
            `shouldBe`
            xfp_correct
        it "fp even sides" $
            (xfp (mkPoly hex) (1.2 @@ rad))
            `shouldBe`
            hex_correct
        it "HA gen" $
            (form_HA square_ha)
	        `shouldBe`
	        "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n<sspaceex xmlns=\"http://www-verimag.imag.fr/xml-namespaces/sspaceex\"version=\"0.2\" math=\"SpaceEx\">\n\t<component id=\"test\">\n\t<param name=\"p1\" type=\"real\" local=\"true\" d1=\"1\" d2=\"1\" dynamics=\"any\" />\n\t<param name=\"p2\" type=\"real\" local=\"true\" d1=\"1\" d2=\"1\" dynamics=\"any\" />\n\t<location id=\"1\" name=\"loc1\">\n \t\t<invariant>inv</invariant>\n\t\t<flow>flow</flow>\n\t</location>\n\t<location id=\"2\" name=\"loc2\">\n \t\t<invariant>inv</invariant>\n\t\t<flow>flow</flow>\n\t</location>\n\t<transition source=\"1\" target=\"2\">\n\t\t<label>t1</label>\n\t\t<guard>guard</guard>\n\t\t<assignment>assignment</assignment>\n\t</transition>\n</component>\n</sspaceex>"
--
--
--
