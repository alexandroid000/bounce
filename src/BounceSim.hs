{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts           #-}

module BounceSim
    (
      Poly(..)
    , BounceState(..)
    , plotBounce
    , pts2poly
    , nextBounce
    , animate
    ) where

import              Diagrams.Prelude
import              Diagrams.TwoD.Segment (lineSegment)
--import            Diagrams.Backend.SVG.CmdLine
import              Diagrams.Backend.Cairo.CmdLine
import              Data.List (minimumBy)
import              Maps

-- Trails are a fundamental data type in Diagrams. They're collections of
-- segments, stored in finger trees
-- actual polygon data type in Diagrams is not that useful
type Poly v n = Located (Trail v n)

type GifDelay = Int

-- state could be either an index for the edge we're on, or a parameter "s" on
-- the perimeter of the polygon
data BounceState = EdgeIndx Int | S Double
    deriving (Eq, Show)

-- take format in Maps.hs -> Poly datatype
pts2poly :: [(Double,Double)] -> Poly V2 Double
pts2poly p = fromVertices (map p2 $ p ++ [head p]) `at` p2 (0,0)

-- all non-self intersections of a vector and polygon
-- result is [(s1, s2, pt)] where
-- s1 is parameter on vector bounce path
-- s2 is parameter on polygon
-- p2 is point of intersection
linePoly :: Double -> Poly V2 Double -> Located (V2 Double) -> [(Double, Double, P2 Double)]
linePoly eps poly bounce =
    let notSelf (s1, s2, p) = s1 > 0.0
        segs = zip [0..] $ fixTrail poly
        n = fromIntegral $ length segs
        reParam k (s1,s2,p) = (s1,(s2 + k)/n,p)
        doBounce (k, seg) = map (reParam k) $ lineSegment eps bounce seg
    in  filter notSelf $ concatMap doBounce segs

-- find closest intersection
closest_int :: (Ord n) => [(n, n, P2 n)] -> (n, n, P2 n)
closest_int = minimumBy (\(s1,_,_) (s2,_,_) -> compare s1 s2)

-- tolerance in case statement below: too low and the program takes forever, too
-- high and we miss some collisions
collision :: Poly V2 Double -> Located (V2 Double) -> BounceState
collision poly bounce =
    let   (s_bounce, s_poly, pt) = case (linePoly 1e-6 poly bounce) of
                                        [] -> error "no intersections? try lower eps"
                                        ints -> closest_int ints
    in    S s_poly

-- bounce is rotated wrt wall we're colliding with
mkBounce :: Point V2 Double -> Angle Double -> V2 Double -> Located (V2 Double)
mkBounce pt ang seg = (seg `at` pt) # rotateAround pt ang

nextBounce :: Poly V2 Double -> Angle Double -> BounceState -> BounceState
nextBounce poly ang (S s)
    = let start_point = P $ (unLoc poly) `atParam` s
          wallSeg = tangentAtParam (unLoc poly) s
      in  collision poly $ mkBounce start_point ang wallSeg

-- chain together many bounces, given list of angles to bounce at
doBounces :: Poly V2 Double -> BounceState -> [Angle Double] -> [BounceState]
doBounces poly = scanl (flip $ nextBounce poly)

mkBounceArrows :: Poly V2 Double -> [Double] -> Double -> Int -> [Diagram B]
mkBounceArrows p angs s num =
    let start = S s
        bounces = doBounces p start $ map (@@ rad) angs
        mkArrows (S s1, S s2) = arrowBetween (p `atParam` s1) (p `atParam` s2)
                                    # lc red
    in  take num $ map mkArrows $ zip bounces (tail bounces)


-- make static diagram of all bounces
plotBounce :: Poly V2 Double -> [Double] -> Double -> Int -> Diagram B
plotBounce p angs s num =
    let bounces = mkBounceArrows p angs s num
    in  (mconcat bounces) `atop` (strokeLocTrail p)

mkFrames :: Poly V2 Double -> [Diagram B] -> [(QDiagram Cairo V2 Double Any, GifDelay)]
mkFrames _ [] = []
mkFrames p arrows = 
    let awtime = map (\a -> (a, 1)) arrows
    in  scanl (\(a,_) (b,_) -> (mconcat [a, b], 100)) (strokeLocTrail p # lc white, 100) awtime

-- GIFS?!?!?!
animate :: Poly V2 Double -> [Double] -> Double -> Int -> [(QDiagram Cairo V2 Double Any, GifDelay)]
animate p angs s num =
    let bounces = mkBounceArrows p angs s num
    in  mkFrames p bounces
