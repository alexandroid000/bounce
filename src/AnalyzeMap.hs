{-# LANGUAGE FlexibleInstances           #-}
module AnalyzeMap
    (
      mkChart
    , mkStar
    ) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Diagrams.Prelude
import Control.Monad
import BounceSim (BounceState(..), Poly, pts2path, nextBounce)
import Maps

log_map :: Double -> Double -> Double
log_map r xn = r*xn*(1-xn)

bounce_map :: Poly Double -> Angle Double -> Double -> Double
bounce_map poly ang xn =
    let (S xnext) = nextBounce poly ang (S xn)
    in  xnext

scanPerimeter :: (Double -> Double) -> [(Double, Double)]
scanPerimeter f =
    let     xs = [0.01, 0.06..0.96]
    in      [ (x, f x) | x <- xs ]

mkStar :: Poly Double
mkStar = pts2path Maps.star

mkScan :: Poly Double -> Angle Double -> [(Double, Double)]
mkScan poly ang = scanPerimeter (bounce_map poly ang)

mkRecurr :: Poly Double -> Angle Double -> [(Double, Double)]
mkRecurr poly ang =
    let bounceSequence = iterate (bounce_map poly ang) 0.1
    in  take 100 $ zip bounceSequence (tail bounceSequence)

-- given a polygon and list of bounce angles to try (ie: [pi/4, pi/2, 3*pi/4])
-- writes resulting recurrence plot in svg to file
mkChart :: Poly Double -> [Double] -> String -> IO ()
mkChart poly queryAngs chartType =
    let angles = map (@@ rad) queryAngs
        chartF = case chartType of
                    "scan" -> (\_ a -> plot $ points (show a) $ mkScan poly a)
                    "recurr" -> (\_ a -> plot $ line (show a) $ [mkRecurr poly a])
    in  toFile def "one_step.svg" $ do
        foldM chartF () angles
        plot $ line "y=x" [scanPerimeter id]
