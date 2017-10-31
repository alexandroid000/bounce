import Animate
import Maps
import BounceSim
import Diagrams.Backend.CmdLine
import Diagrams.Backend.Cairo.CmdLine


main =  do
--        angs <- randAngs
        let angs = repeat $ 0.6
-- choose map from Maps.hs
        let map = oct_shear
        gifRender (dOpts, gOpts) $ animate (mkPoly map) angs 0.4 200
