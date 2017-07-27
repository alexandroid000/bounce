{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE FlexibleContexts  #-}

module GenDiagrams where

import Diagrams.Prelude
import BounceSim
import Maps
--import Animate
import Diagrams.Backend.CmdLine
--import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.SVG

dOpts = DiagramOpts
            { _width = Just 400
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

-- uhhh
--fpPoints :: Double -> Int -> Int -> Double -> Diagram B
--fpPoints theta n' m' l =
--    let env = regPoly n' l :: Trail V2 Double
--        n = fromIntegral n'
--        m = fromIntegral m'
--        th = case theta > pi/2 of
--                True -> -(theta-pi/2)
--                False -> pi/2 - theta
--        a = (l*(sin (pi*(m+1)/n))*(sin (m*pi/n)))/((sin (pi/n))*(sin (pi*(n-2*m)/n)))
--        c = case theta > pi/2 of
--                True -> (cos th)/(cos (th + (pi*(n-2*m)/n)))
--                False -> (cos th)/(cos (th - (pi*(n-2*m)/n)))
--        fx = case theta > pi/2 of
--                True -> (l*c + a*(1-c))/(1+c)
--                False -> (l-a*(1-c))/(1+c)
--        s_fx = case n' `mod` m' of
--                0 -> map (\i -> 1/n + m*i/n + fx/(l*n)) [0..n-1]
--                r -> map (\i -> ((fromIntegral ((i*m') `mod` n'))/n + fx/(n*l)))
--                                [0..n-1]
--        fps = map (\s -> origin .+^ (atParam env s)) s_fx
--     in (visPoints fps)

--plotGenFP :: Double -> Int -> Int -> Double -> Diagram B
plotGenFP theta n' m' l =
    let env = regPoly n' l :: Trail V2 Double
        n = fromIntegral n'
        sim = snd $ plotBounce (mkPoly $ Trl env) (repeat theta) (0.5) 100
        --fps = fpPoints theta n' m' l
    in sim -- <> fps


mkBounceArrows :: Poly V2 Double ->
                    [RoboLoc] -> Int -> Colour Double -> [Diagram B]
mkBounceArrows p bounces num col =
    let transparentList = 1 : (map (*1.0) transparentList)
        getMask len = reverse $ take len transparentList
        mkOpaque arrows = zipWith opacity (getMask (length arrows)) arrows
        mkArrows (s1, s2) = arrowBetween' (with & headLength .~ large)
                                (p `atParam` s1) (p `atParam` s2) # lc col
    in  mkOpaque $ take num $ map mkArrows $ zip bounces (tail bounces)

-- make static diagram of all bounces
-- Pair with list of impact points
--plotBounce :: Poly V2 Double -> [Double] -> Double -> Int -> ([RoboLoc], Diagram B)
plotBounce p angs s num =
    let bounces = doBounces p s $ map (@@ rad) angs :: [RoboLoc]
        --start_pt = circle 15 # fc green # lc blue # moveTo (p `atParam` s)
        arrows = mkBounceArrows p bounces num blue
        plot =  (mconcat arrows # lwL 5) <>
                (strokeLocTrail p # lwL 11) 
    in  (bounces, plot)

