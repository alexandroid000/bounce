{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module GenDiagrams where

import Diagrams.Prelude
import BounceSim
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

--visPoints :: [P2 Double] -> QDiagram b V2 Double Any
--visPoints pts = atPoints pts (repeat ((circle 15) # fc yellow # lc blue))

getPoints :: Poly V2 Double -> [Double] -> [P2 Double]
getPoints p ss = map (atParam p) ss


-- [(Int, Double)] is the edge index and local edge parameter of collision point
--putPoints :: Poly V2 Double -> [(Int, Double)] -> Int -> QDiagram b V2 Double Any
putPoints poly collisions offset = let
    segs = trailLocSegments poly
    vecs = trailOffsets $ unLoc poly
    lens = map (\x -> x `dot` x) vecs :: [Double]
    normed_colls = zipWith (\(i,s) y -> (i,s/y)) collisions lens
    indexed_segs = fromList $ zip [0..] segs :: Map Int (Located (Segment Closed V2 Double))
    in indexed_segs


--plotGenFP :: Double -> Int -> Int -> Double -> Diagram B
plotGenFP theta n' m' l =
    let env = regPoly n' l :: Trail V2 Double
        n = fromIntegral n'
        sim = snd $ plotBounce (mkPoly $ Trl env) doFixedBounce (repeat theta) (0.5) 100
        --fps = fpPoints theta n' m' l
    in sim -- <> fps


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
    let bounces = doBounces p bounceLaw s $ map (@@ rad) angs :: [RoboLoc]
        --start_pt = circle 15 # fc green # lc blue # moveTo (p `atParam` s)
        arrows = mkBounceArrows p bounces num blue
        plot =  (mconcat arrows # lwL 5) <>
                (strokeLocTrail p # lwL 10)
    in  (bounces, plot)

