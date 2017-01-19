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
    , rand  :: Double   -- magnitude of random angle (default 0)
    }

sim :: Options.Applicative.Parser Simulation
sim = Simulation
        <$> strOption
                    (   short 'o' <>
                        long "output" <>
                        metavar "FILENAME" <>
                        help "file name / path (svg most likely)"
                    )
        <*> strOption
                    (   short 'e' <>
                        long "environment" <>
                        metavar "ENV_NAME" <>
                        value "star" <>
                        help "name of environment in Maps.hs"
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
                        help "angle to bounce at"
                    )
        <*> option auto
                    (   short 's' <>
                        long "start" <>
                        metavar "START_PARAM" <>
                        value 0.23 <>
                        help "parameter in interval (0,1)"
                    )
        <*> option auto -- default 0
                    (   short 'r' <>
                        long "random" <>
                        metavar "RAND_ADD" <>
                        value 0.0 <>
                        help "random offset added to theta"
                    )

-- return uniform random value in [-max_r, +max_r]
randAngs :: Double -> IO [Double]
randAngs max_r = do
    g <- getStdGen
    return (randomRs (-max_r + 0.001,max_r-0.001) g :: [Double])

-- returns max range of random value. depends on theta
-- clamps to maximum possible range if user-given value is too high
clamped_r :: Double -> Double -> Double
clamped_r ang rand
        | rand < (pi-ang) && rand < ang = rand
        | otherwise = min (pi-ang) ang

runSim :: Simulation -> IO ()
runSim (Simulation fname env num ang s rand) = do
        rangs <- randAngs (clamped_r ang rand)
        let mkAngs ang
                | 0 < ang && ang < pi = repeat $ ang
                | otherwise = error "angle not between 0 and pi"
        let angs = zipWith (+) rangs (mkAngs ang)
        let map = mkPoly $ maps ! env
        let pxsize = (mkSizeSpec2D (Just 400) Nothing)
        let (plot_dat, sim) = plotBounce map angs s num
        print $ show (take num plot_dat)
        writeFile "bounce_data.txt" (show $ take num plot_dat)
        renderSVG fname pxsize $ sim
 --       renderSVG fname pxsize $ plotLimitCycle 0.2 5 300

-- main looks weird because it has boilerplate to make cmd line parser
-- feeds everything to runSim
main :: IO ()
main = execParser opts >>= runSim
    where   opts = info (helper <*> sim)
                    (   fullDesc
                    <>  progDesc "creates diagram of  simulation at FILENAME"
                    )
