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
