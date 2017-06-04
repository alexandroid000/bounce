module Animate
    ( animate
    ) where

import Diagrams.Prelude
import BounceSim
import GenDiagrams
import Maps
import Diagrams.Backend.CmdLine
import Diagrams.Backend.Cairo.CmdLine

type GifDelay = Int


-- GIFS?!?!?!
mkFrames :: Poly V2 Double -> [Diagram B] -> [(QDiagram Cairo V2 Double Any, GifDelay)]
mkFrames _ [] = []
mkFrames p arrows =
    let awtime = map (\a -> (a, 1)) arrows
    in  scanl (\(a,_) (b,_) -> (mconcat [b, a], 25))
        (strokeLocTrail p # lc black # bgFrame 2 white, 25) awtime

animate :: Poly V2 Double -> [Double] -> Double -> Int -> [(QDiagram Cairo V2 Double Any, GifDelay)]
animate p angs s num =
    let bounces = doBounces p s $ map (@@ rad) angs
        arrows =
            mkBounceArrows p bounces num blue
    in  mkFrames p arrows


--main =  do
----        angs <- randAngs
--        let angs = repeat $ 0.6
---- choose map from Maps.hs
--        let map = oct_shear
--        gifMain $ animate (mkPoly map) angs 0.4 200
