{-# LANGUAGE NoMonomorphismRestriction  #-}

module GenMapFns where

import Diagrams.Prelude
import BounceSim
import Maps

edgeLen :: V2 Double -> Double
edgeLen v = sqrt (v `dot` v)

polyOffsets :: Poly V2 Double -> [V2 Double]
polyOffsets p = trailOffsets $ unLoc p

polyLens :: Poly V2 Double -> [Double]
polyLens = map edgeLen . polyOffsets

-- find th that rotates frame so that v1 is along x axis
angleInPoly :: V2 Double -> V2 Double -> Angle Double
angleInPoly v1 v2 = let
    th = (0 @@ rad) ^-^ (v1 ^. _theta)
    phi = (rotate th v2) ^. _theta
    in (pi @@ rad) ^-^ phi

cyclicPairs :: [a] -> [(a,a)]
cyclicPairs l = (zip l $ tail l) ++ [(last l, head l)]

polyAngs :: Poly V2 Double -> [Angle Double]
polyAngs p = let
    offs = polyOffsets p
    offset_pairs = cyclicPairs offs
    get_ang (v1, v2) = angleInPoly v1 v2
    in map get_ang offset_pairs

polyLenAngs :: Poly V2 Double -> [(Double, Angle Double)]
polyLenAngs p = zip (polyLens p) (polyAngs p)

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

--bounceFromPt :: Poly V2 Double -> Angle Double -> V2 Double -> V2 Double
--bounceFromPt poly theta x =
--    let e1 = closest_edge poly x
--        e2 = case (lht theta) of
--                True -> ccw poly e1
--                False -> cw poly e1
--    in seq_bounce theta (e1,e2)


