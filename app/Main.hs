module Main where

import BounceSim
import System.Random
import Maps
import Diagrams.Backend.SVG.CmdLine

randAngs :: IO [Double]
randAngs = do
    g <- getStdGen
    return (randomRs (0.001,pi-0.001) g :: [Double])

main =  do
--        angs <- randAngs
        let angs = repeat $ pi/3
        mainWith $ plotBounce (pts2path Maps.bigpoly) angs


