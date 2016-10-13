{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts           #-}

module BounceSim
    (
      Poly
    , RoboLoc
    , Robot
    , MapSpec
    , plotBounce
    , pts2poly
    , mkPoly
    , mkBounce
    , doBounce
    , lineSeg
    , linePoly
    , shootRay
    , animate
    ) where

import              Diagrams.Prelude
import              Diagrams.TwoD.Segment           (lineSegment)
import              Diagrams.Trail                  (trailPoints)
import              Diagrams.Backend.SVG.CmdLine
--import              Diagrams.Backend.Cairo.CmdLine
import              Data.List                       (minimumBy)
import              Maps

-- Trails are a fundamental data type in Diagrams. They're collections of
-- segments, stored in finger trees
-- actual polygon data type in Diagrams is not that useful
type Poly v n = Located (Trail v n)

type GifDelay = Int

-- state could be either an index for the edge we're on, or a parameter "s" on
-- the perimeter of the polygon
type RoboLoc = Double

-- (location (param on dS polygon boundary), heading vector, environment)
type Robot = (RoboLoc, V2 Double, Poly V2 Double)


-- Helper Functions
-- ----------------


-- take format in Maps.hs -> Poly datatype
mkPoly :: MapSpec -> Poly V2 Double
mkPoly (Pts ps) = pts2poly $ map p2 ps
mkPoly (Trl tr) = tr `at` origin

pts2poly :: [Point V2 Double] -> Poly V2 Double
pts2poly p = (closeTrail $ trailFromVertices p) `at` p2 (0,0)

-- change from parameter on segment (0,1) to parameter on polygon
reParam :: Int -> Int -> Double -> Double
reParam n k s =
        let (nd, kd) = (fromIntegral n, fromIntegral k) :: (Double, Double)
        in  (s + kd)/nd

-- Geometry Functions
-- ------------------

-- rotate a vector around a point
mkBounce :: Point V2 Double -> Angle Double -> V2 Double -> V2 Double
mkBounce pt ang vec = vec # rotateAround pt ang

-- compare one line and one edge of a polygon
-- returns list of correctly parameterized intersection points
-- filter out intersections at origin of line (for bounces starting at edge)
lineSeg :: Double -> Int -> Int -> Located (V2 Double) -> FixedSegment V2 Double -> [(Double, Double)]
lineSeg eps n k bounce seg =
    let notSelf (s1, s2) = (abs s1) > eps
        intersect_param (s1,s2,p) = (s1, reParam n k s2)
        find_intersect = filter notSelf . map intersect_param
    in  find_intersect (lineSegment eps bounce seg)

-- all non-self intersections of a vector and polygon
-- result is [(s1, s2)] where
-- s1 is parameter on vector bounce path
-- s2 is parameter on polygon
linePoly :: Double -> Poly V2 Double -> Located (V2 Double) -> [(Double, Double)]
linePoly eps poly bounce =
    let segs = zip [0..] $ fixTrail poly
        n = fromIntegral $ length segs
    in  concatMap (\(k,s) -> lineSeg eps n k bounce s) segs

-- just a wrapper for the case where you know you'll have intersections
-- set tolerance lower to have fewer errors, higher for faster execution
shootRay :: Poly V2 Double -> Located (V2 Double) -> RoboLoc
shootRay poly bounce =
    let mkPos (s1, s2)
            | s1 > 0    = True
            | otherwise = False
        (s_bounce, s_poly) = case (linePoly 1e-6 poly bounce) of
                                    []   -> error "no intersections? try lower eps"
                                    ints -> minimum $ filter mkPos ints
    in  s_poly

doBounce :: Angle Double -> Robot -> Robot
doBounce theta (s,b,p) =
    let pt = p `atParam` s
        tangentV = tangentAtParam p s
        new_bounce = mkBounce pt theta tangentV
        new_s = shootRay p $ new_bounce `at` pt
    in  (new_s, new_bounce, p)


-- chain together many bounces, given list of angles to bounce at
doBounces :: Poly V2 Double -> RoboLoc -> [Angle Double] -> [RoboLoc]
doBounces poly s1 angs =
    let start = (s1, unitX, poly) -- I don't like this unitX placeholder
        nextBounce robo a = doBounce a robo
    in  map (\(s,_,_) -> s) $ scanl nextBounce start angs

-- Diagram Generators
-- ------------------

visPoints :: [P2 Double] -> Diagram B
visPoints pts = atPoints pts (repeat (circle 5 # lw none # fc blue))

mkBounceArrows :: Poly V2 Double -> [Double] -> Double -> Int -> [Diagram B]
mkBounceArrows p angs s num =
    let start = s
        bounces = doBounces p start $ map (@@ rad) angs
        transparentList = 1 : (map (*0.95) transparentList)
        getMask len = reverse $ take len transparentList
        mkOpaque arrows = zipWith opacity (getMask (length arrows)) arrows
        mkArrows (s1, s2) = arrowBetween (p `atParam` s1) (p `atParam` s2)
                                    # lc red
    in  mkOpaque $ take num $ map mkArrows $ zip bounces (tail bounces)

-- make static diagram of all bounces
plotBounce :: Poly V2 Double -> [Double] -> Double -> Int -> Diagram B
plotBounce p angs s num =
    let bounces = mkBounceArrows p angs s num
    in  (mconcat bounces) `atop` (strokeLocTrail p)

-- GIFS?!?!?!
--mkFrames :: Poly V2 Double -> [Diagram B] -> [(QDiagram Cairo V2 Double Any, GifDelay)]
mkFrames _ [] = []
mkFrames p arrows = 
    let awtime = map (\a -> (a, 1)) arrows
    in  scanl (\(a,_) (b,_) -> (mconcat [a, b], 100)) (strokeLocTrail p # lc white, 100) awtime

--animate :: Poly V2 Double -> [Double] -> Double -> Int -> [(QDiagram Cairo V2 Double Any, GifDelay)]
animate p angs s num =
    let bounces = mkBounceArrows p angs s num
    in  mkFrames p bounces
