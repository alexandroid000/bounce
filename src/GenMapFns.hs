{-# LANGUAGE NoMonomorphismRestriction  #-}

module GenMapFns where

import Diagrams.Prelude
import BounceSim
import Maps

polyOffsets :: Poly V2 Double -> [V2 Double]
polyOffsets p = trailOffsets $ unLoc p

polyLens :: Poly V2 Double -> [Double]
polyLens = map (\v -> sqrt (v `dot` v)) . polyOffsets

-- rotate so v1 is along x axis
-- then two cases for reflex and non-reflex angles
angleInPoly :: V2 Double -> V2 Double -> Angle Double
angleInPoly v1' v2' = let
    th = (0 @@ rad) ^-^ (v1' ^. _theta)
    v1 = rotate th v1'
    v2 = rotate th v2'
    phi = v2 ^. _theta
    in (pi @@ rad) ^-^ phi

polyAngs :: Poly V2 Double -> [Angle Double]
polyAngs p = let
    offs = polyOffsets p
    offset_pairs = (zip offs (tail offs)) ++ [(last offs, head offs)]
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
seq_bounce theta ((l1,phi1), (l2, phi2)) = let
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


phi = 5*pi/7 @@ rad
th = 1.2 @@ rad
c = coeff th phi
xfp_correct = 500.0*c/(1+c)

test = do
    print $ xfp (mkPoly hep) (1.2 @@ rad)
    print $ xfp_correct

