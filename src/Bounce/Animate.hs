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
mkFrames p traj = scanl
                  (\(a,_) b -> (mconcat [b, a], 25))
                  (strokeLocTrail p # lc black # bgFrame 2 white, 25)
                  traj

mkGif :: [Diagram B] -> [(Diagram B, GifDelay)]
mkGif frames = map (\f -> (f, 25)) frames

animate = animateTraj
--animate = animateShear

animateShear =
    let ts = map shearingX [0.0, 0.05 .. 1.5]
    in animateEnvs ts

mkTraj :: SimState -> [Diagram B]
mkTraj plotOpts =
    let p = poly plotOpts
        blaw = bounce plotOpts
        s = ss plotOpts
        ans = angs plotOpts
        num = n plotOpts
        bounces = doBounces p blaw s ans
    in  mkGifArrows plotOpts bounces blue


animateTraj :: SimState -> [(QDiagram Cairo V2 Double Any, GifDelay)]
animateTraj plotOpts =
    let p = poly plotOpts
        bounces = mkTraj plotOpts
    in  mkFrames p bounces

animateEnvs :: [Transformation V2 Double] -> SimState -> [(QDiagram Cairo V2 Double Any, GifDelay)]
animateEnvs trans plotOpts =
    let p_orig = poly plotOpts
        drawP p = strokeLocTrail p # lc black # bgFrame 2 white
        p_seq = map (\t -> transform t p_orig) trans
        opts_seq = map (\p -> plotOpts { poly = p }) p_seq
        p_plots = map drawP p_seq
        bounces = map (\opts -> mconcat $ mkTraj opts) opts_seq
        plots = zipWith (<>) bounces p_plots
    in  mkGif plots
