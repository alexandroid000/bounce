{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

edge :: Located (Trail V2 Double)
edge = fromOffsets [2 *^ unitX] `at` origin

pt :: Point V2 Double
pt = atParam edge 0.5

normV :: V2 Double
normV = normalAtParam edge 0.5 # rotate (pi @@ rad)

inc :: V2 Double
inc = normV # rotate (0.8 @@ rad)

out :: V2 Double
out = normV # rotate ((-0.8) @@ rad)

bounce :: V2 Double -> Diagram B
bounce v = fromOffsets [v]

ang :: Diagram B
ang = let p1 = pt .+^ (0.5 *^ out)
          p2 = pt .+^ (0.5 *^ inc)
      in  strokeLocTrail $ arcBetween p1 p2 (-0.15) `at` p1

bounce_def :: Diagram B
bounce_def = strokeLocTrail edge # lwN 0.01
            <> mconcat
            [ bounce inc,
            bounce normV # dashingN [0.03,0.03] 0,
            arrowV out] # moveTo pt
            <> ang

frameT :: Located (Trail V2 Double)
frameT = triangle 100 # scaleY 1.2 `at` origin

node :: String -> String -> Diagram B
node txt dir =
    let mkPt :: String -> V2 Double -> Diagram B
        mkPt txt dr = mconcat [circle 2 # lw none # fc black,
                                baselineText txt # translate dr # italic # fontSize
                                25]
    in  case dir of
            "u" -> mkPt txt unitY
            "d" -> mkPt txt (unitY # rotateBy 0.5)
            "l" -> mkPt txt (20 *^ unitX # rotateBy 0.5)
            "r" -> mkPt txt (5 *^ unitX)

bnce_line :: Located (Trail V2 Double) -> Diagram B
bnce_line tri =
    let p1 = tri `atParam` 0.07
        p2 = tri `atParam` 0.5
    in arrowBetween p1 p2
        <> node "x" "r" # moveTo p1
        <> node "f(x)" "l" # moveTo p2

ed :: Double -> Double -> Located (Trail V2 Double) -> Diagram B
ed s1 s2 tri =
    let p0 = tri `atParam` s1
        p1 = tri `atParam` s2
    in mconcat [p0 ~~ p1 # lwN 0.01, circle 2 # moveTo p0 # lw none # fc black,
    circle 2 # moveTo p1 # lw none # fc black ]


gen_bounce :: Diagram B
gen_bounce = (strokeLocTrail frameT # dashingN [0.02, 0.02] 0
                <> bnce_line frameT
                <> ed 0.0 0.2 frameT
                <> ed 0.466 0.666 frameT) # centerXY
                # pad 1.2

main = mainWith gen_bounce
