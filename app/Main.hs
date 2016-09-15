{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import              System.Random
import              Diagrams.Backend.SVG             (renderSVG)
import              Diagrams.TwoD.Size               (mkSizeSpec2D)

-- local libraries
import              BounceSim
import              Refine                           (refine)
import              Maps

-- CLI
import              Options.Applicative

-- yaml
import              Control.Applicative
import              Data.Yaml
import qualified    Data.ByteString.Char8 as BS

-- edit this to run simulation with different map
simMap = Maps.equiltri

-- yaml parsing, still not working
data Map = Map { map :: Double}
    deriving (Show, Read)

instance FromJSON Map where
    parseJSON (Object v) =  Map <$>
                            v .: "map"
    parseJSON _ = error "Can't parse map"

-- parse command line arguments

data Simulation = Simulation
    { fname :: String
    , num   :: Int
    , ang   :: Double
    , s     :: Double
    , rand  :: Bool
    }

sim :: Options.Applicative.Parser Simulation
sim = Simulation
        <$> strOption
                    (   short 't' <>
                        long "write-to" <>
                        metavar "FILENAME" <>
                        help "file name / path"
                    )
        <*> option auto
                    (   short 'n' <>
                        long "num" <>
                        metavar "NUM_BOUNCES" <>
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
                        help "parameter in interval (0,1)"
                    )
        <*> switch -- default false
                    (   short 'r' <>
                        long "random" <>
                        help "true -> random bounce angles"
                    )

randAngs :: IO [Double]
randAngs = do
    g <- getStdGen
    return (randomRs (0.001,pi-0.001) g :: [Double])

runSim :: Simulation -> IO ()
runSim (Simulation fname num ang s rand) = do
--        ymlData <- BS.readFile "Maps.yaml"
--        let map = Data.Yaml.decode ymlData :: Maybe Map
--        print $ maybe (error "parsefail") (show) map
        rangs <- randAngs
        let angs = case rand of
                        True    -> rangs
                        _       -> repeat $ ang
        let map = mkPoly simMap
        let pxsize = (mkSizeSpec2D (Just 400) Nothing)
        renderSVG fname pxsize $ plotBounce map angs s num

-- main looks weird because it has boilerplate to make cmd line parser
-- feeds everything to runSim
main :: IO ()
main = execParser opts >>= runSim
    where   opts = info (helper <*> sim)
                    (   fullDesc
                    <>  progDesc "creates diagram of  simulation at FILENAME"
                    )
