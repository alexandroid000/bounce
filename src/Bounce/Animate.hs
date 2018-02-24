{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}


module Bounce.Animate
    ( animate
    ) where

import Diagrams.Prelude
import Bounce.Simulate
import Bounce.Diagrams
import Diagrams.Environments
import Diagrams.Backend.CmdLine
import Diagrams.Backend.Cairo.CmdLine

type GifDelay = Int

-- doBounces ::    Poly V2 Double -> BounceFunct ->
--                 RoboLoc -> [Angle Double] -> [RoboLoc]

mkGifArrows :: SimState -> [RoboLoc] -> Colour Double -> [Diagram B]
mkGifArrows plotOpts bounces col =
    let p = poly plotOpts
        num = n plotOpts
        mkArrows (s1, s2) = arrowBetween' (with & headLength .~ large)
                                (p `atParam` s1) (p `atParam` s2) # lc col
        arrows = take num $ map mkArrows $ zip bounces (tail bounces)
     in arrows

-- GIFS?!?!?!
mkFrames :: Poly V2 Double -> [Diagram B] -> [(Diagram B, GifDelay)]
mkFrames _ [] = []
mkFrames p arrows =
    let --transparentList = 1 : (map (*1.0) transparentList)
        --getMask n = reverse $ take n transparentList
        --mkOpaque ar = zipWith opacity (getMask (length arrows)) ar
        --shaded_ars = mkOpaque arrows
        awtime = map (\a -> (a, 1)) arrows
    in  scanl (\(a,_) (b,_) -> (mconcat [b, a], 25))
        (strokeLocTrail p # lc black # bgFrame 2 white, 25) awtime

animate :: SimState -> [(QDiagram Cairo V2 Double Any, GifDelay)]
animate plotOpts =
    let p = poly plotOpts
        blaw = bounce plotOpts
        s = ss plotOpts
        ans = angs plotOpts
        num = n plotOpts
        bounces = doBounces p blaw s ans
        arrows = mkGifArrows plotOpts bounces blue
    in  mkFrames p arrows


--main =  do
--    let angs = repeat $ 0.6
---- choose map from Maps.hs
--    let map = oct_shear
--    gifMain $ animate (mkPoly map) doFixedBounce angs 0.4 200
