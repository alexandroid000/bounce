{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts           #-}

module Bounce.Simulate
    (
      Poly
    , RoboLoc
    , Robot
    , BounceFunct
    , MapSpec
    , pts2poly
    , mkPoly
    , mkBounce
    , doFixedBounce
    , doFixedSpeBounce
    , doRelativeBounce
    , doSpecBounce
    , doBounces
    , lineSeg
    , linePoly
    , shootRay
    , randAngs
    ) where

import              Diagrams.Prelude
import              Diagrams.TwoD.Segment           (lineSegment)
import qualified    Diagrams.Trail                  (trailPoints)

import              Diagrams.Environments

import              Data.List                       (minimumBy)
import              System.Random

-- Trails are a fundamental data type in Diagrams. They're collections of
-- segments, stored in finger trees
-- actual polygon data type in Diagrams is not that useful
type Poly v n = Located (Trail v n)


-- state could be either an index for the edge we're on, or a parameter "s" on
-- the perimeter of the polygon
type RoboLoc = Double

-- (location (param on dS polygon boundary), heading vector, environment)
type Robot = (RoboLoc, V2 Double, Poly V2 Double)

-- takes angle param, state of world, returns new state
type BounceFunct = Angle Double -> Robot -> Robot

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

-- return uniform random value in [-max_r, +max_r]
randAngs :: Double -> IO [Double]
randAngs max_r = do
    g <- getStdGen
    return (randomRs (-max_r, max_r) g :: [Double])

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
shootRay :: Poly V2 Double -> Located (V2 Double) -> Maybe RoboLoc
shootRay poly bounce =
    let mkPos (s1, s2)
            | s1 > 0    = True
            | otherwise = False
        s_poly = case (linePoly 1e-6 poly bounce) of
                        []   -> Nothing
                        ints -> case (filter mkPos ints) of
                            [] -> Nothing
                            ss -> Just $ snd $ minimum ss
    in  s_poly

-- bounce at a fixed angle, relative to wall normal, regardless of incoming traj
doFixedBounce :: BounceFunct
doFixedBounce theta (s,b,p) =
    let pt = p `atParam` s
        tangentV = tangentAtParam p s
        theta' = (pi/2 -(theta ^. rad)) @@ rad
        new_bounce = mkBounce pt theta' tangentV
        new_s = shootRay p $ new_bounce `at` pt
    in  maybe   (error "no intersections? try lower eps")
                (\s -> (s, new_bounce, p)) new_s

-- bounce at a fixed angle, relative to wall normal, in the direction of increasing x
doFixedSpeBounce :: BounceFunct
doFixedSpeBounce theta (s,b,p) =
    let pt = p `atParam` s
        tangentV = tangentAtParam p s
        ang = (angleBetween tangentV b) ^. rad
        n_ang = (pi/2 - ang)/abs (pi/2 - ang)
        theta' = (pi/2 -(abs(theta ^. rad)*n_ang)) @@ rad
        new_bounce = mkBounce pt theta' tangentV
        new_s = shootRay p $ new_bounce `at` pt
    in  maybe   (error "no intersections? try lower eps")
                (\s -> (s, new_bounce, p)) new_s
-- rotate through a fixed angle, relative to incoming traj, when collide with
-- wall
-- possible to escape polygon!! No safety checks yet TODO
doRelativeBounce :: BounceFunct
doRelativeBounce theta (s,b,p) =
    let pt = p `atParam` s
        new_bounce = mkBounce pt theta b
        new_s = shootRay p $ new_bounce `at` pt
        correct_b = case new_s of
                        Just s -> (s, new_bounce, p)
                        Nothing -> doRelativeBounce theta (s, new_bounce, p)
    in correct_b

-- bounce like a pool ball or laser beam
doSpecBounce :: BounceFunct
doSpecBounce _ (s,b,p) =
    let pt = p `atParam` s
        tangentV = tangentAtParam p s
        ang = angleBetween tangentV b
        new_bounce = mkBounce pt ang tangentV
        new_s = shootRay p $ new_bounce `at` pt
    in  maybe   (error "no intersections? try lower eps")
                (\s -> (s, new_bounce, p)) new_s

-- need to refactor bounce law
-- chain together many bounces, given list of angles to bounce at
doBounces ::    Poly V2 Double -> BounceFunct -> RoboLoc -> [Angle Double] -> [RoboLoc]
doBounces poly bounceLaw s1 angs =
    let start = (s1, rotate (45 @@ deg) (unitY :: V2 Double), poly) -- I don't like this unitX placeholder
        nextBounce robo a = bounceLaw a robo
    in  map (\(s,_,_) -> s) $ scanl nextBounce start angs
