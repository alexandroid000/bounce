module Main where

import BounceSim
import System.Random
import Maps
import Diagrams.Backend.SVG.CmdLine

randAngs :: IO [Double]
randAngs = do
    g <- getStdGen
    return (randomRs (0.001,pi-0.001) g :: [Double])

-- edit angle below to generate an infinite list of bounces at that angle or
-- uncomment "angs <- randAngs" and comment out the line below it to do random
-- bounces
main =  do
--        angs <- randAngs
        let angs = repeat $ pi/3
-- choose map from Maps.hs
        let map = Maps.bigpoly
        mainWith $ plotBounce (pts2path map) angs


