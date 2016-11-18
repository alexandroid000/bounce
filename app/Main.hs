{-# LANGUAGE DeriveGeneric #-}

module Main where

import              System.Random
import              Diagrams.Backend.SVG             (renderSVG)
import              Diagrams.TwoD.Size               (mkSizeSpec2D)

-- local libraries
import              BounceSim
import              Refine                           (refine)
import              Maps                             (maps)

-- CLI
import              Options.Applicative
import              Data.HashMap


data Simulation = Simulation
    { fname :: String   -- filename
    , env   :: String   -- environment (polygon)
    , num   :: Int      -- number of bounces
    , ang   :: Double   -- angle to bounce at (-pi/2, pi/2)
    , s     :: Double   -- perimeter parameter to start at
    , rand  :: Bool     -- true/false random bounces (overrides angle)
    , px    :: Double      -- pixel width of diagram
    }

sim :: Options.Applicative.Parser Simulation
sim = Simulation
        <$> strOption
                    (   short 'o' <>
                        long "output" <>
                        metavar "FILENAME" <>
                        help "output file name / path (svg most likely)"
                    )
        <*> strOption
                    (   short 'e' <>
                        long "environment" <>
                        metavar "ENV_NAME" <>
                        value "star" <>
                        help "name of environment (found at top of Maps.hs)"
                    )
        <*> option auto
                    (   short 'n' <>
                        long "num" <>
                        metavar "NUM_BOUNCES" <>
                        value 10 <>
                        help "number of bounces"
                    )
        <*> option auto
                    (   short 'a' <>
                        long "angle" <>
                        metavar "ANGLE" <>
                        value   0.2 <>
                        help "bounce angle wrt tangent vector (0,pi)"
                    )
        <*> option auto
                    (   short 's' <>
                        long "start" <>
                        metavar "START_PARAM" <>
                        value 0.23 <>
                        help "parameter in interval (0,1)"
                    )
        <*> switch -- default false
                    (   short 'r' <>
                        long "random" <>
                        help "if flag presence, creates random bounce angles"
                    )
        <*> option auto
                    (   short 'p' <>
                        long "px" <>
                        metavar "PIXEL_WIDTH" <>
                        value 400 <>
                        help "pixel width of output image"
                    )

randAngs :: IO [Double]
randAngs = do
    g <- getStdGen
    return (randomRs (0.001,pi-0.001) g :: [Double])

runSim :: Simulation -> IO ()
runSim (Simulation fname env num ang s rand px) = do
        rangs <- randAngs
        let angs = case rand of
                        True    -> rangs
                        _       -> repeat $ ang
        let map = mkPoly $ maps ! env
        let pxsize = (mkSizeSpec2D (Just px) Nothing)
        renderSVG fname pxsize $ plotBounce map angs s num

-- main looks weird because it has boilerplate to make cmd line parser
-- feeds everything to runSim
main :: IO ()
main = execParser opts >>= runSim
    where   opts = info (helper <*> sim)
                    (   fullDesc
                    <>  progDesc "creates diagram of  simulation at FILENAME"
                    )
