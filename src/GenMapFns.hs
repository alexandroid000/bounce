{-# LANGUAGE NoMonomorphismRestriction  #-}

module GenMapFns where

import Diagrams.Prelude
import BounceSim
import Maps

polyOffsets :: Poly V2 Double -> [V2 Double]
polyOffsets p = trailOffsets $ unLoc p

polyLens :: Poly V2 Double -> [Double]
polyLens p = let
    offs = polyOffsets p
    norm v = sqrt (v `dot` v)
    in map norm offs

polyAngs :: Poly V2 Double -> [Angle Double]
polyAngs p = let
    offs = polyOffsets p
    offset_pairs = (zip offs (tail offs)) ++ [(last offs, head offs)]
    get_ang (v1, v2) = angleBetween v1 v2
    in map get_ang offset_pairs

polyLenAngs :: Poly V2 Double -> [(Double, Angle Double)]
polyLenAngs p = zip (polyLens p) (polyAngs p)
