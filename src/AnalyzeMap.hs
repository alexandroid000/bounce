{-# LANGUAGE FlexibleInstances           #-}

-- utils for analyzing maps of form x_{n+1} = f(x_n)

module AnalyzeMap
    (
      mkChart
    , mkStar
    ) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Diagrams.Prelude
import Control.Monad
import BounceSim (Poly, mkPoly, doBounce)
import Maps

log_map :: Double -> Double -> Double
log_map r xn = r*xn*(1-xn)

bounce_map :: Poly V2 Double -> Angle Double -> Double -> Double
bounce_map poly ang xn =
    let (xnext,_,_) = doBounce ang (xn, unitX, poly)
    in  xnext

scanPerimeter :: (Double -> Double) -> [(Double, Double)]
scanPerimeter f =
    let     xs = [0.01, 0.06..0.96]
    in      [ (x, f x) | x <- xs ]

mkStar :: Poly V2 Double
mkStar = mkPoly Maps.star

mkScan :: Poly V2 Double -> Angle Double -> [(Double, Double)]
mkScan poly ang = scanPerimeter (bounce_map poly ang)

-- generates infinite list
-- see https://en.wikipedia.org/wiki/Poincaré_plot
mkReturnMap :: (Double -> Double) -> Double -> [(Double, Double)]
mkReturnMap f x0 =
    let xs = iterate f x0
    in  zip xs (tail xs)

-- generates infinite list
-- see https://en.wikipedia.org/wiki/Cobweb_plot
mkCobweb :: (Double -> Double) -> Double -> [(Double,Double)]
mkCobweb f x0 =
    let fx0 = f x0
    in  (x0, fx0):(fx0,fx0):(mkCobweb f fx0)

-- given a polygon and list of bounce angles to try (ie: [pi/4, pi/2, 3*pi/4])
-- writes resulting recurrence plot in svg to file
mkChart :: Poly V2 Double -> [Double] -> Double -> Int -> String -> String -> IO ()
mkChart poly queryAngs start num chartType fname =
    let angles = map (@@ rad) queryAngs
        bFn a = bounce_map poly a
        chartF = case chartType of
                    "scan"      ->  (\_ a -> plot $ points (show a) $
                                    mkScan poly a)
                    "return"    ->  (\_ a -> plot $ line (show a) $
                                    [take num $ mkReturnMap (bFn a) start])
                    "cobweb"    ->  (\_ a -> plot $ line (show a) $
                                    [take num $ mkCobweb (bFn a) start])
                    _           ->  error "chart type not defined"
    in  toFile def fname $ do
        foldM chartF () angles
        plot $ line "y=x" [scanPerimeter id]
