{-# LANGUAGE DeriveGeneric #-}

module Main where

import BounceSim
import System.Random
import Maps
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Refine
import GHC.Generics

randAngs :: IO [Double]
randAngs = do
    g <- getStdGen
    return (randomRs (0.001,pi-0.001) g :: [Double])

-- edit angle below to generate an infinite list of bounces at that angle or
-- uncomment "angs <- randAngs" and comment out the line below it to do random
-- bounces
main :: IO ()
main =  do
--        angs <- randAngs
        let angs = repeat $ pi/2
-- choose map from Maps.hs
        let map = refine (mkPoly Maps.poly1) (pi/2 @@ rad)
--        let map = pts2poly Maps.poly1
        mainWith $ plotBounce map angs


