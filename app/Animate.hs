module AnimateBounce where

import BounceSim
import Maps
import Diagrams.Backend.Cairo.CmdLine

main =  do
--        angs <- randAngs
        let angs = repeat $ pi/2
-- choose map from Maps.hs
        let map = Maps.star
        gifMain $ animate (pts2path map) angs 0.2 15
