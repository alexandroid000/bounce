{-# LANGUAGE NoMonomorphismRestriction  #-}

module GenMapFns where

import Diagrams.Prelude
import Diagrams.TwoD.Segment
import BounceSim
import Data.Ord
import Data.List
import Maps

cyclicPairs :: [a] -> [(a,a)]
cyclicPairs l = (zip l $ tail l) ++ [(last l, head l)]

edgeLen :: V2 Double -> Double
edgeLen v = sqrt (v `dot` v)

-- find th that rotates frame so that v1 is along x axis
edgesAngle :: V2 Double -> V2 Double -> Angle Double
edgesAngle v1 v2 = let
    th = (0 @@ rad) ^-^ (v1 ^. _theta)
    phi = (rotate th v2) ^. _theta
    in (pi @@ rad) ^-^ phi

edgesLenAng :: (V2 Double, V2 Double) -> (Double, Angle Double)
edgesLenAng (v1,v2) = (edgeLen v1, edgesAngle v1 v2)

polyOffsets :: Poly V2 Double -> [V2 Double]
polyOffsets p = trailOffsets $ unLoc p

polyLens :: Poly V2 Double -> [Double]
polyLens = map edgeLen . polyOffsets

polyLenAngs :: Poly V2 Double -> [(Double, Angle Double)]
polyLenAngs p = let
    offs = cyclicPairs . trailOffsets $ unLoc p
    in map edgesLenAng offs

-- Calculating fixed point for sequential edge bouncing in convex polygons
-- seems to have a sign error for even-sided polygons
-- need to test with non-regular polygons and get plotting working

coeff :: Angle Double -> Angle Double -> Double
coeff theta phi = (cosA theta)/(cosA (theta ^-^ phi))

seq_bounce :: Angle Double -> ((Double, Angle Double), (Double, Angle Double)) -> (Double -> Double)
seq_bounce theta ((l1,phi1), (l2, _)) = let
    c = coeff theta phi1
    in \x -> c*(l2 - x)

coeff_prod :: Angle Double -> [(Double, Angle Double)] -> Double
coeff_prod theta lenangs = let
    coeffs = map (\(l,p) -> coeff theta p) lenangs
    in foldr (*) 1.0 coeffs

xfpNumerator :: Int -> Int -> Angle Double -> [(Double, Angle Double)] -> Double
xfpNumerator (-1) _ _ _ = 0.0
xfpNumerator i n theta la@((li, phi):lenangs) = let
    sign_l = li*(-1)^(n-1-i)
    in sign_l*(coeff_prod theta la) + (xfpNumerator (i-1) n theta lenangs)

xfpDenom n theta lenangs = let
    in 1 - (coeff_prod theta lenangs)*(-1)^n

xfp :: Poly V2 Double -> Angle Double -> Double
xfp poly theta = let
    lenangs = polyLenAngs poly
    n = length lenangs
    in (xfpNumerator (n-1) n theta lenangs)/(xfpDenom n theta lenangs)

closestSegment :: [FixedSegment V2 Double] -> P2 Double -> (Int, FixedSegment V2 Double)
closestSegment segs' pt =
    let segs = zip [0..] segs'
        closerToPt e1 e2 = comparing (\(i,e) -> closestDistance e pt) e1 e2
    in minimumBy (closerToPt) segs

bounceFromPt :: Poly V2 Double -> Angle Double -> P2 Double -> (Double -> Double)
bounceFromPt poly theta x =
    let segs = fixTrail poly
        vs = polyOffsets poly
        n = length segs
        (i,e1) = closestSegment segs x
        v1 = vs !! i
        v2 = case (theta < (pi/2 @@ rad)) of
                True -> vs !! ((i+1) `mod` n)
                False -> vs !! ((i-1) `mod` n)
    in seq_bounce theta (edgesLenAng (v1,v2), edgesLenAng (v2, unitX))

collisionPts :: Poly V2 Double -> Angle Double -> [P2 Double]
collisionPts poly theta =
    let segs = fixTrail poly
        lens = polyLens poly
        fst_fp = xfp poly theta
        s1 = (fst_fp / (head lens))*(1.0/(fromIntegral $ length lens))
    in [poly `atParam` s1]
