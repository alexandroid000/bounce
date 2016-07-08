{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts           #-}

module BounceSim
    (
      Poly(..)
    , BounceState(..)
    , plotBounce
    , pts2path
    , nextBounce
      
    ) where

import Diagrams.Prelude
import Diagrams.TwoD.Segment (lineSegment)
import Diagrams.Backend.SVG.CmdLine
import Data.List (minimumBy)
import Maps

type Poly n = Located (Trail V2 n)

data BounceState = EdgeIndx Int | S Double
    deriving (Eq, Show)

-- make poly generator and output in most efficient format
pts2path :: [(Double,Double)] -> Poly Double
pts2path p = fromVertices (map p2 $ p ++ [head p]) `at` p2 (0,0)

-- all non-self intersections of a vector and polygon
-- result is [(s1, s2, pt)] where
-- s1 is parameter on vector bounce path
-- s2 is parameter on polygon
-- p2 is point of intersection
linePoly :: Double -> Poly Double -> Located (V2 Double) -> [(Double, Double, P2 Double)]
linePoly eps poly bounce =
    let notSelf (s1, s2, p) = s1 > eps
        segs = zip [0..] $ fixTrail poly
        n = fromIntegral $ length segs
        reParam k (s1,s2,p) = (s1,(s2 + k)/n,p)
        doBounce (k, seg) = map (reParam k) $ lineSegment eps bounce seg
    in  filter notSelf $ concatMap doBounce segs

-- find closest intersection
closest_int :: (Ord n) => [(n, n, P2 n)] -> (n, n, P2 n)
closest_int = minimumBy (\(s1,_,_) (s2,_,_) -> compare s1 s2)

collision :: Poly Double -> Located (V2 Double) -> BounceState
collision poly bounce =
    let   (s_bounce, s_poly, pt) = case (linePoly 1e-6 poly bounce) of
                                        [] -> error "no intersections?"
                                        ints -> closest_int ints
    in    S s_poly

-- bounce is rotated wrt wall we're colliding with
mkBounce :: Point V2 Double -> Angle Double -> Segment Closed V2 Double -> Located (V2 Double)
mkBounce pt ang seg = ((segOffset seg) `at` pt) # rotateAround pt ang

nextBounce :: Poly Double -> Angle Double -> BounceState -> BounceState
nextBounce poly ang (S s)
    = let start_point = P $ (unLoc poly) `atParam` s
          GetSegmentCodomain bounce = (getSegment $ unLoc poly) `atParam` s
          (offset, incSeg, s_iso) = maybe (error "bounce undefined") id bounce
          mkCollide b = collision poly b
      in  mkCollide $ mkBounce start_point ang incSeg

doBounces :: Poly Double -> BounceState -> [Angle Double] -> [BounceState]
doBounces poly = scanl (flip $ nextBounce poly)

plotBounce :: Poly Double -> [Double] -> Double -> Int -> Diagram B
plotBounce p angs s num =
    let start = S s
        bounces = doBounces p start $ map (@@ rad) angs
        mkArrows (S s1, S s2) = arrowBetween (p `atParam` s1) (p `atParam` s2)
                                    # lc red
        bounceTrail = take num $ map mkArrows $ zip bounces (tail bounces)
    in  (mconcat bounceTrail) `atop` (strokeLocTrail p)
