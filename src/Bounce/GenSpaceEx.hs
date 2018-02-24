module GenHA where

import HA
import Diagrams.Prelude
import BounceSim
import Maps
import Numeric           (showFFloat)

type Seg = (P2 Double, P2 Double)



showN :: Double -> String
showN n = "("++(Numeric.showFFloat Nothing n "")++")"

--mk_HA :: Poly V2 Double -> Angle Double -> HA
mk_pairs poly = let
    pts = (trailVertices . mapLoc cutTrail) poly
    mkpairs [] = []
    mkpairs [a] = []
    mkpairs (a:b:ls) = (a,b):(mkpairs (b:ls))
    in mkpairs pts

data Orientation = Vert | Horiz | Neither

detect_orientation :: Seg -> Orientation
detect_orientation (pt1, pt2)
    | abs (x1-x2) < eps = Vert
    | abs (y1-y2) < eps = Horiz
    | otherwise         = Neither
    where   eps = 0.001
            (x1,y1) = unp2 pt1
            (x2,y2) = unp2 pt2


check_on_hv p1 p2 p =
    case p1 < p2 of
        True  -> (showN p1)++" &lt;= "++p++" &amp;&amp; "++p++" &lt; "++(showN p2)
        False -> (showN p2)++" &lt; "++p++" &amp;&amp; "++p++" &lt;= "++(showN p1)

mkGuard :: Seg -> String
mkGuard (pt1, pt2) =
    let eps = 0.001
        (x1,y1) = unp2 pt1
        (x2,y2) = unp2 pt2
        s1 = "(x-("++(showN x1)++"))/("++(showN x2)++" - ("++(showN x1)++"))"
        s2 = "(y-("++(showN y1)++"))/("++(showN y2)++" - ("++(showN y1)++"))"
    in  case detect_orientation (pt1, pt2) of
            Vert  -> "x - "++(showN x1)++" &lt; "++(showN eps)++" &amp;&amp; x - "++
                        (showN x1)++" &gt; -"++(showN eps)++" &amp;&amp; "++(check_on_hv y1 y2 "y")
            Horiz -> "y - "++(showN y1)++" &lt; "++(showN eps)++" &amp;&amp; y - "++
                        (showN y1)++" &gt; -"++(showN eps)++" &amp;&amp; "++(check_on_hv x1 x2 "x")
            _     -> "("++s1++"-"++s2++") "++"&lt; "++(showN eps) ++" &amp;&amp; "++
                     "("++s1++"-"++s2++") "++"&gt; -"++(showN eps) ++" &amp;&amp; "++
                     "(0.0 &lt;= "++s1++") &amp;&amp; ("++s1++"&lt; 1.0)"

-- since we arbitrarily set |v| = 1, vx = cos(theta) and vy = sin(theta)
-- thus, when we update theta to theta_edge + theta_controller, we can use the
-- sin and cosine sum rules to compute new vx, vy without using inverse trig
mkAssign :: Angle Double -> Seg -> Assignment
mkAssign theta (pt1, pt2) = let
    (x1,y1) = unp2 pt1
    (x2,y2) = unp2 pt2
    len_e = sqrt ((y2-y1)^2 + (x2-x1)^2)
    sin_the = (y2-y1)/len_e
    cos_the = (x2-x1)/len_e
    theta' = (pi/2 -(theta ^. rad))
    cos_thc = cos theta'
    sin_thc = sin theta'
    cos_thout = cos_the*cos_thc - sin_the*sin_thc
    sin_thout = sin_the*cos_thc + cos_the*sin_thc
    in "vx := "++(showN cos_thout)++" &amp; vy := "++(showN sin_thout)


mkT :: Angle Double -> String -> Seg -> Transition
mkT theta label seg =
    let guard = mkGuard seg
        assign = mkAssign theta seg
    in Transition 1 1 label guard assign

mkTs :: Poly V2 Double -> Angle Double -> [Transition]
mkTs poly theta = let
    edges = mk_pairs poly
    n = length edges
    labels = mkLabels n
    in map (uncurry (mkT theta)) (zip labels edges)

mkLabels :: Int -> [String]
mkLabels n = map (\i -> "e"++(show i)) [1..n]

mkParams :: Poly V2 Double -> [Param]
mkParams poly = let
    dyn_params = [RealDyn "x", RealDyn "y", RealConst "vx", RealConst "vy"]
    edges = trailLocSegments poly
    n = length edges
    edge_params = map (\lab -> Lab lab) (mkLabels n)
    in dyn_params ++ edge_params
