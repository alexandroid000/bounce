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
import BounceSim (BounceState(..), Poly, pts2poly, nextBounce)
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
mkStar = pts2poly Maps.star

mkScan :: Poly Double -> Angle Double -> [(Double, Double)]
mkScan poly ang = scanPerimeter (bounce_map poly ang)

-- generates infinite list
-- see https://en.wikipedia.org/wiki/Poincaré_plot
mkReturnMap :: (Double -> Double) -> Double -> [(Double, Double)]
mkReturnMap f x0 =
    let xs = iterate f x0
    in  zip xs (tail xs)

-- generates infinite list
-- see https://en.wikipedia.org/wiki/Cobweb_plot
mkCobweb :: Poly Double -> Angle Double -> Double -> [(Double,Double)]
mkCobweb poly ang s0 =
    let bmap = bounce_map poly ang
        fx0 = bmap s0
        ffx0 = bmap fx0
    in  (s0, fx0):(fx0,fx0):(fx0,ffx0):(ffx0,ffx0):(mkCobweb poly ang ffx0)

-- given a polygon and list of bounce angles to try (ie: [pi/4, pi/2, 3*pi/4])
-- writes resulting recurrence plot in svg to file
mkChart :: Poly Double -> [Double] -> Int -> String -> String -> IO ()
mkChart poly queryAngs num chartType fname =
    let angles = map (@@ rad) queryAngs
        start = 0.3
        bFn a = bounce_map poly a
        chartF = case chartType of
                    "scan"      ->  (\_ a -> plot $ points (show a) $
                                    mkScan poly a)
                    "recurr"    ->  (\_ a -> plot $ line (show a) $
                                    [take num $ mkReturnMap (bFn a) start])
                    "cobweb"    ->  (\_ a -> plot $ line (show a) $
                                    [take num $ mkCobweb poly a start])
                    _           ->  error "chart type not defined"
    in  toFile def fname $ do
        foldM chartF () angles
        plot $ line "y=x" [scanPerimeter id]
