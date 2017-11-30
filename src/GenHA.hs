module GenHA where

import HA
import Diagrams.Prelude
import BounceSim
import Maps

type Seg = (P2 Double, P2 Double)

loc1 = Location 1 "interior" "" "x'=vx &amp; y'=vy"
t1 = Transition 1 1 "e1" "guard" "assignment"

-- theta_e: angle between edge e and global x-axis
-- theta_c: control input (angle to leave edge e at)
-- vx = cos(theta)
-- vy = sin(theta)
-- vx^2 + vy^2 = 1 (invariant, normalize / check for floating point err)
-- updates: theta := theta_e + theta_c
-- cos(theta) := cos(theta_e)cos(theta_c)-sin(theta_e)sin(theta_c)
-- sin(theta) := sin(theta_e)cos(theta_c)+cos(theta_e)sin(theta_c)


--mk_HA :: Poly V2 Double -> Angle Double -> HA
mk_pairs poly = let
    pts = (trailVertices . mapLoc cutTrail) poly
    mkpairs [] = []
    mkpairs [a] = []
    mkpairs (a:b:ls) = (a,b):(mkpairs (b:ls))
    in mkpairs pts

is_on_edge :: Seg -> String
is_on_edge (pt1, pt2) =
    let eps = 0.001
        (x1,y1) = unp2 pt1
        (x2,y2) = unp2 pt2
        s1 = "(x-("++(show x1)++"))/("++(show x2)++" - ("++(show x1)++"))"
        s2 = "(y-("++(show y1)++"))/("++(show y2)++" - ("++(show y1)++"))"
    in "|"++s1++"-"++s2++"| "++"&lt; "++(show eps) ++" && "++"(0.0 &lt;= "++s1++") && ("++s1++"&lt; 1.0)"

mkBounceTran :: String -> Seg -> Transition
mkBounceTran label (pt1, pt2) =
    let guard = is_on_edge (pt1, pt2)
        assign = "assign"
    in Transition 1 1 label guard assign

mkBounceTrans :: Poly V2 Double -> [Transition]
mkBounceTrans poly = let
    edges = mk_pairs poly
    n = length edges
    labels = map (\n -> "e"++(show n)) [1..n]
    in map (uncurry mkBounceTran) (zip labels edges)


-- rotates incoming angle to new one after bounce
-- angles \in [-pi,pi], relative to x axis
rotate_relative_to_edge :: Angle Double -> Angle Double -> Seg -> Angle Double
rotate_relative_to_edge alpha theta (pt1, pt2) = let
    edge_vect = pt2 .-. pt1
    theta' = (pi/2 -(theta ^. rad)) @@ rad
    bounce_vect = edge_vect # rotate theta'
    in signedAngleBetween bounce_vect unitX


mkAssign :: Seg -> String
mkAssign seg = let
    


pt = p2 (1,1) :: P2 Double
pt1 = p2 (0,0) :: P2 Double
pt2 = p2 (2,2) :: P2 Double

test = mkBounceTrans (mkPoly pent)
