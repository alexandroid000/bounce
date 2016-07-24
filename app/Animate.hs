module AnimateBounce where

import BounceSim
import Maps
import System.Random
import Diagrams.Backend.Cairo.CmdLine

randAngs :: IO [Double]
randAngs = do
    g <- getStdGen
    return (randomRs (0.001,pi-0.001) g :: [Double])

main =  do
--        angs <- randAngs
        let angs = repeat $ pi/2
-- choose map from Maps.hs
        let map = Maps.star
        gifMain $ animate (pts2poly map) angs 0.2 15
