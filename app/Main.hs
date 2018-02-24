{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where
        
import              System.Random
import              Diagrams.Prelude                (Diagram, V2, rad, (@@), Angle)
import              Diagrams.TwoD.Size              (mkSizeSpec2D)
import              Diagrams.TwoD.Align             (centerXY)
import              Diagrams.Combinators            (pad)
import              Diagrams.Util

-- local libraries
import              Bounce.Simulate
import              Bounce.Diagrams
import              Bounce.Animate                   (animate)
import              Diagrams.Environments            (maps)

-- CLI
import              Options.Applicative
import              Data.HashMap
import              Data.Monoid
import              Diagrams.Backend.SVG
import              Diagrams.Backend.CmdLine
import              Diagrams.Backend.Cairo.CmdLine  (GifOpts(..), gifRender)

data BLaw = Fixed
          | Relative
          | Specular
          | FixedSpe
    deriving (Read, Show)

data Simulation = Simulation
    { fname :: String   -- filename
    , env   :: String   -- environment (polygon)
    , num   :: Int      -- number of bounces
    , ang   :: Double   -- angle to bounce at (-pi/2, pi/2)
    , blaw  :: BLaw     -- bouncing law
    , s     :: Double   -- perimeter parameter to start at
    , rand  :: Double   -- magnitude of random angle (default 0)
    , gif   :: Bool     -- produce gif output
    }

gOpts = GifOpts
      { _dither = False
      , _noLooping = False
      , _loopRepeat = Nothing
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
                        long "env" <>
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
                    (   short 'b' <>
                        long "bouncelaw" <>
                        metavar "BLAW" <>
                        value   Fixed <>
                        help "fixed, relative, or specular bouncing"
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
        <*> switch -- default false
                    (   short 'g' <>
                        long "gif" <>
                        help "create animated gif"
                    )


-- returns max range of random value. depends on theta
-- clamps to maximum possible range if user-given value is too high
clamped_r :: Double -> Double -> Double
clamped_r ang rand
        | (abs rand) < (pi/2-(abs ang)) = abs rand
        | otherwise = (pi/2) - (abs ang)

generate :: BE -> String -> Double -> SimState -> IO ()
generate (Cair) fname width plotOpts = let
        w_int   = round width
        opts    = (DiagramOpts (Just w_int) Nothing fname, gOpts)
        sim     = animate plotOpts
    in  gifRender opts sim
--generate (Cairo) _ _ _ _ _ _ _ = error "sorry, gif functionality borked"

generate (Svg) fname width plotOpts = let
        pxsize          = (mkSizeSpec2D (Just width) Nothing)
        (plot_dat, sim) = plotBounce plotOpts
    in renderSVG fname pxsize $ (sim # centerXY # pad 1.2)

runSim :: Simulation -> IO ()
runSim (Simulation fname env num ang blaw s rand gif) = do
        rangs <- randAngs (clamped_r ang rand)
        let mkAngs ang
                | (-pi/2) < ang && ang < (pi/2) = repeat $ ang
                | otherwise = error "angle not between -pi/2 and pi/2"
        let ans = Prelude.map (@@ rad) $ zipWith (+) rangs (mkAngs ang)
        let map = mkPoly $ maps ! env
        let bfunct = case blaw of
                          Fixed -> doFixedBounce
                          Relative -> doRelativeBounce
                          Specular -> doSpecBounce
                          FixedSpe -> doFixedSpeBounce
        let plotOpts = SimState
                     { poly = map
                     , bounce = bfunct
                     , ss = s
                     , angs = ans
                     , n = num
                     }
        case gif of
            True ->     generate Cair fname 400.0 plotOpts
            False ->    generate Svg fname 400.0 plotOpts

-- feeds everything to runSim
main :: IO ()
main = execParser opts >>= runSim
    where   opts = info (helper <*> sim)
                    (   fullDesc
                    <>  progDesc "creates diagram of  simulation at FILENAME"
                    )
