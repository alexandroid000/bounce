module EdgeGraph where

import              Diagrams.Prelude
import              Diagrams.Backend.SVG.CmdLine
import              Refine

-- A vertex is a (named) black circle of radius 0.2.

vertex :: Int -> Diagram B
vertex i = circle 0.2 # fc white # named i

-- A K_{2} consists of two vertices joined with an edge of length one.

-- decoratePath does not creat a line between the vertices it only draws
-- the object at the vertices so use an arrow with no head.
k2 :: Diagram B
k2 = dia # connectOutside' (with & arrowHead .~ noHead)  (1 :: Int) (2 :: Int)
  where dia = atPoints (regPoly 3 1) [vertex 1, vertex 2, vertex 3]

main :: IO ()
main = mainWith k2
