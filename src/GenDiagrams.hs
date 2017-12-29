{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module GenDiagrams where

import Diagrams.Prelude
import BounceSim
import Maps
import GenMapFns
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

getPoints :: Poly V2 Double -> [Double] -> [P2 Double]
getPoints p ss = map (atParam p) ss

--plotGenFP :: Double -> Int -> Int -> Double -> Diagram B
plotGenFP theta n' m' l =
    let env = regPoly n' l :: Trail V2 Double
        n = fromIntegral n'
        sim = snd $ plotBounce (mkPoly $ Trl env) doFixedBounce (repeat theta) (0.5) 100
    in sim


mkBounceArrows :: Poly V2 Double ->
                    [RoboLoc] -> Int -> Colour Double -> [Diagram B]
mkBounceArrows p bounces num col =
    let transparentList = 1 : (map (*0.98) transparentList)
        getMask len = reverse $ take len transparentList
        mkOpaque arrows = zipWith opacity (getMask (length arrows)) arrows
        mkArrows (s1, s2) = arrowBetween' (with & headLength .~ large)
                                (p `atParam` s1) (p `atParam` s2) # lc col
    in  mkOpaque $ take num $ map mkArrows $ zip bounces (tail bounces)

-- make static diagram of all bounces
-- Pair with list of impact points
--plotBounce :: Poly V2 Double -> [Double] -> Double -> Int -> ([RoboLoc], Diagram B)
plotBounce p bounceLaw angs s num =
    let bounces = doBounces p bounceLaw s $ map (@@ rad) angs :: [RoboLoc]
        --start_pt = circle 15 # fc green # lc blue # moveTo (p `atParam` s)
        arrows = mkBounceArrows p bounces num blue
        th = (head angs) @@ rad
        pts = bounceFromPt p th (fstPoint p th)
        plot = (mconcat arrows # lwL 3) <>
               (strokeLocTrail p # lwL 5) <>
               (visPoints pts)
                
    in  (bounces, plot)

