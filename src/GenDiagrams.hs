{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module GenDiagrams where

import Diagrams.Prelude
import BounceSim
import GenMapFns
import Maps
import Data.HashMap hiding (map)
--import Animate
import Diagrams.Backend.CmdLine
--import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.SVG

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
plotGenFP poly theta =
    let sim = snd $ plotBounce poly doFixedBounce (repeat theta) (0.5) 100
        fps = fpPoints poly theta
    in fps <> sim


mkBounceArrows :: Poly V2 Double ->
                    [RoboLoc] -> Int -> Colour Double -> [Diagram B]
mkBounceArrows p bounces num col =
    let transparentList = 1 : (map (*0.99) transparentList)
        getMask len = reverse $ take len transparentList
        mkOpaque arrows = zipWith opacity (getMask (length arrows)) arrows
        mkArrows (s1, s2) = arrowBetween' (with & headLength .~ large)
                                (p `atParam` s1) (p `atParam` s2) # lc col
    in  mkOpaque $ take num $ map mkArrows $ zip bounces (tail bounces)

-- make static diagram of all bounces
-- Pair with list of impact points
--plotBounce :: Poly V2 Double -> [Double] -> Double -> Int -> ([RoboLoc], Diagram B)
plotBounce p bounceLaw angs s num =
    let bounces = doBounces p bounceLaw s angs :: [RoboLoc]
        --start_pt = circle 15 # fc green # lc blue # moveTo (p `atParam` s)
        arrows = mkBounceArrows p bounces num blue
        plot =  (mconcat arrows # lwL 5) <>
                (strokeLocTrail p # lwL 10)
    in  (bounces, plot)

