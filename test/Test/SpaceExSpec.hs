{-# LANGUAGE QuasiQuotes #-}

module Test.SpaceExSpec where
        
import Test.Hspec

import Text.RawString.QQ

import Diagrams.Prelude
import Diagrams.Environments
import Bounce.Simulate
import Bounce.SpaceEx
import Bounce.GenSpaceEx

loc1 = Location 1 "interior" "-500.0 &lt;= x &amp;&amp; x &lt;= 0.0 &amp;&amp; 0 &lt;= y &amp;&amp; y &lt;= 500" "x'==vx &amp; y'==vy"

square_ha :: HA
square_ha = HA   { name = "test"
               , params = mkParams $ mkPoly sq
               , locations = [loc1]
               , transitions = mkTs (mkPoly sq) ((pi/2)-0.05 @@ rad)
               }

spec :: Spec
spec = do
    describe "SpaceEx Tests" $ do
        it "HA gen" $
            (form_HA square_ha)
            `shouldBe`
            [r|<?xml version="1.0" encoding="iso-8859-1"?>
<sspaceex xmlns="http://www-verimag.imag.fr/xml-namespaces/sspaceex"version="0.2" math="SpaceEx">
	<component id="test">
	<param name="x" type="real" local="true" d1="1" d2="1" dynamics="any" />
	<param name="y" type="real" local="true" d1="1" d2="1" dynamics="any" />
	<param name="vx" type="real" local="true" d1="1" d2="1" dynamics="const" />
	<param name="vy" type="real" local="true" d1="1" d2="1" dynamics="const" />
	<param name="e1" type="label" local="false" />
	<param name="e2" type="label" local="false" />
	<param name="e3" type="label" local="false" />
	<param name="e4" type="label" local="false" />
	<location id="1" name="interior">
 		<invariant>-500.0 &lt;= x &amp;&amp; x &lt;= 0.0 &amp;&amp; 0 &lt;= y &amp;&amp; y &lt;= 500</invariant>
		<flow>x'==vx &amp; y'==vy</flow>
	</location>
	<transition source="1" target="1" asap="true" >
		<label>e1</label>
		<guard>x - (0.0) &lt; (0.001) &amp;&amp; x - (0.0) &gt; -(0.001) &amp;&amp; (0.0) &lt;= y &amp;&amp; y &lt; (500.0)</guard>
		<assignment>vx := (-0.049979169270678483) &amp; vy := (0.9987502603949663)</assignment>
	</transition>
	<transition source="1" target="1" asap="true" >
		<label>e2</label>
		<guard>y - (500.0) &lt; (0.001) &amp;&amp; y - (500.0) &gt; -(0.001) &amp;&amp; (-500.00000000000006) &lt; x &amp;&amp; x &lt;= (-0.00000000000005551115123125783)</guard>
		<assignment>vx := (-0.9987502603949663) &amp; vy := (-0.04997916927067837)</assignment>
	</transition>
	<transition source="1" target="1" asap="true" >
		<label>e3</label>
		<guard>x - (-500.00000000000006) &lt; (0.001) &amp;&amp; x - (-500.00000000000006) &gt; -(0.001) &amp;&amp; (0.0) &lt; y &amp;&amp; y &lt;= (500.0)</guard>
		<assignment>vx := (0.04997916927067826) &amp; vy := (-0.9987502603949663)</assignment>
	</transition>
	<transition source="1" target="1" asap="true" >
		<label>e4</label>
		<guard>y - (0.0) &lt; (0.001) &amp;&amp; y - (0.0) &gt; -(0.001) &amp;&amp; (-500.0000000000001) &lt;= x &amp;&amp; x &lt; (0.0)</guard>
		<assignment>vx := (0.9987502603949663) &amp; vy := (0.04997916927067837)</assignment>
	</transition>
</component>
</sspaceex>|]
