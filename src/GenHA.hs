module GenHA where

import HA
import Diagrams.Prelude
import BounceSim
import Maps


loc1 = Location 1 "interior" "" "x'=vx &amp; y'=vy"
t1 = Transition 1 1 "e1" "guard" "assignment"

--mk_HA :: Poly V2 Double -> Angle Double -> HA
mk_HA p ang = let
    pts = (trailVertices . mapLoc cutTrail) p
    mkpairs [] = []
    mkpairs [a] = []
    mkpairs (a:b:ls) = (a,b):(mkpairs (b:ls))
    in mkpairs pts

is_on_edge :: (P2 Double, P2 Double) -> String
is_on_edge (pt1, pt2) =
    let eps = 0.001
        (x1,y1) = unp2 pt1
        (x2,y2) = unp2 pt2
        s1 = "(x-"++(show x1)++")/("++(show x2)++" - "++(show x1)++")"
        s2 = "(y-"++(show y1)++")/("++(show y2)++" - "++(show y1)++")"
    in "|"++s1++"-"++s2++"| "++"&lt; "++(show eps) ++" && "++"(0.0 &lt;= "++s1++") && ("++s1++"&lt; 1.0)"

mkBounceTrans :: (P2 Double, P2 Double) -> Transition
mkBounceTrans (pt1, pt2) =
    let guard = is_on_edge (pt1, pt2)
        assign = "assign"
    in Transition 1 1 "e1" guard assign

pt = p2 (1,1) :: P2 Double
pt1 = p2 (0,0) :: P2 Double
pt2 = p2 (2,2) :: P2 Double

test = mk_HA (mkPoly pent) (0.2 @@ rad)
