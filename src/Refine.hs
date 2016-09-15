{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts           #-}

module Refine
    ( refine
    ) where

import              Diagrams.Prelude
import              Diagrams.TwoD.Path              (isInsideWinding)
import              Diagrams.Trail                  (trailPoints)
import              Data.List                       (sort)
import              BounceSim

-- take abs to find closest point of intersection in either direction from vertex
closest_s :: [(Double, Double)] -> Double
closest_s ss =
    let abss = map (\(s1,s2) -> (abs s1, s2)) ss
    in  snd $ minimum abss

-- discard bounces that don't start on correct segment
-- this allows for nonconvex polygons
on_edge :: Int -> (Int, Double) -> Bool
on_edge ni (ki,s)
    | k/n < s && s < (k+1)/n = True
    | otherwise = False
    where   n = fromIntegral ni
            k = fromIntegral ki

correctPreimages :: Int -> [(Int, Double)] -> [Double]
correctPreimages n ints = map snd $ filter (on_edge n) ints

-- Find bounce from each edge that would intersect point s
bouncePreimages :: Poly V2 Double -> Angle Double -> Point V2 Double -> [(Int, Double)]
bouncePreimages poly ang vertex =
    let edgeVs = trailOffsets (unLoc poly)
        bounce :: (Int, V2 Double) -> (Int, [(Double,Double)])
        bounce (k,edge) =   (k,
                            linePoly 1e-6 poly $
                            (mkBounce vertex ang edge) `at` vertex)
    in  map (\(k,ss) -> (k, closest_s ss)) $
        filter (\(_,ss) -> not (null ss)) $
        map bounce $ zip [0..] edgeVs

-- nonconvexity check by interpolating halfway between bounce start and end,
-- checking if that point is inside polygon. A little hacky but will always work
-- since bouncePreimages finds closest intersect, so entire segment will be
-- either inside or outside polygon
v_intersects :: Poly V2 Double -> Angle Double -> Point V2 Double -> [Double]
v_intersects poly ang vert =
    let n = fromIntegral $ length $ trailPoints poly
        ints = correctPreimages n $ bouncePreimages poly ang vert
        interp sint = lerp 0.5 (poly `atParam` sint) vert
    in  filter ((`isInsideWinding` (toPath poly)) . interp) ints

-- TODO MAKE THIS NOT TERRIBLE
refine :: Poly V2 Double -> Angle Double -> Poly V2 Double
refine poly ang =
    let verts = trailPoints poly
        n = fromIntegral $ length verts
        s_verts = [k/n | k <- [0..n]]
        newverts = concatMap (v_intersects poly ang) verts
        all_verts = sort (s_verts ++ newverts)
        all_points = map (atParam poly) all_verts
    in  pts2poly all_points

splitJoinAt :: Poly V2 Double -> Double -> Poly V2 Double
splitJoinAt poly s =
    let (p1,p2) = (cutTrail $ unLoc poly) `splitAtParam` s
    in  (closeTrail (p1 <> p2)) `at` origin
