{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Bounce.Diagrams where

import Diagrams.Prelude
import Diagrams.Environments
import Diagrams.Backend.CmdLine
import Diagrams.Backend.SVG

import Data.HashMap hiding (map)

import Bounce.Simulate
import Bounce.GenMapFns

data BE = Svg | Cair

--data PolyPlot = C Diagrams.Backend.Cairo.B | S Diagrams.Backend.SVG.B

data SimState = SimState
              { poly    :: Poly V2 Double
              , bounce  :: BounceFunct
              , ss      :: RoboLoc
              , angs    :: [Angle Double]
              , n       :: Int
              }

dOpts = DiagramOpts
            { _width = Just 500
            , _height = Nothing
            , _output = "test.svg"}
--gOpts = GifOpts
--            { _dither = False
--            , _noLooping = False
--            , _loopRepeat = Nothing}

-- Diagram Generators
-- ------------------

visPoints :: [P2 Double] -> Diagram B
visPoints pts = atPoints pts (repeat ((circle 15) # fc yellow # lc blue))

fpPoints :: Poly V2 Double -> Angle Double -> Diagram B
fpPoints poly theta = let
    s1 = fstPoint poly theta
    pts = bounceFromPt poly theta s1
    in visPoints pts

-- only works for convex polygons
--plotGenFP :: Poly V2 Double -> Angle Double -> Diagram B
plotGenFP p theta =
    let pconfig = SimState
                { poly = p
                , bounce = doFixedBounce
                , ss = 0.5
                , angs = repeat theta
                , n = 100
                }
        sim = snd $ plotBounce pconfig
        fps = fpPoints p theta
    in fps <> sim


mkBounceArrows :: SimState -> [RoboLoc] -> Colour Double -> [Diagram B]
mkBounceArrows plotOpts bounces col =
    let p = poly plotOpts
        num = n plotOpts
        transparentList = 1 : (map (*0.99) transparentList)
        getMask len = reverse $ take len transparentList
        mkOpaque arrows = zipWith opacity (getMask (length arrows)) arrows
        arrowhead = (with & headLength .~ large)
        -- use the following if you want more options for arrowheads
        arrowhead2 = (with & headStyle %~ fc blue . lc black . lwL 0.8)
        mkArrows (s1, s2) = arrowBetween' arrowhead
                                (p `atParam` s1) (p `atParam` s2) # lc col
        arrows = mkOpaque $ take num $ map mkArrows $ zip bounces (tail bounces)
     in arrows

-- make static diagram of all bounces
-- Pair with list of impact points
--plotBounce :: Poly V2 Double -> [Double] -> Double -> Int -> ([RoboLoc], Diagram B)
plotBounce :: SimState -> ([RoboLoc], Diagram B)
plotBounce plotOpts =
    let p = poly plotOpts
        bounces = doBounces p (bounce plotOpts) (ss plotOpts) (angs plotOpts)
        arrows = mkBounceArrows plotOpts bounces blue
        plot = (mconcat arrows # lwL 5.0) <> (strokeLocTrail p # lwL 10.0)
    in  (bounces, plot)

