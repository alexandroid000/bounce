{-# LANGUAGE NoMonomorphismRestriction  #-}

-- experiment with using the state monad to compose bounces
-- currently not being used (due to focusing on other things)
-- upside: makes specular bouncing a lot easier, and could implement contracting
-- billiards very easily as well

module Bounce.MonadBounce where

import              Diagrams.Prelude

import              Control.Monad.State
import              Data.HashMap

import              Bounce.Simulate
import              Diagrams.Environments

test = (0.2, unitY, mkPoly $ maps ! "triang")

--nextBounce :: Poly V2 Double -> Angle Double -> RoboLoc -> RoboLoc
--nextBounce poly ang (S s) =
--    let start_point = P $ (unLoc poly) `atParam` s
--        tangentV = tangentAtParam (unLoc poly) s
--    in  shootRay poly $ mkBounce start_point ang tangentV

bounceWrtWall :: Angle Double -> State Robot RoboLoc
bounceWrtWall theta = state $ \robot -> let st@(s,b,p) = doBounce theta robot
                                        in (s, st)

bounceSpecular :: State Robot RoboLoc
bounceSpecular = state $ \robot@(s,b,p) ->
    let tangentV = tangentAtParam p s
        inc_theta = angleBetween tangentV b
        st@(s,b,p) = doBounce inc_theta robot
    in  (s,st)

bouncy :: State Robot RoboLoc
bouncy = do
    s1 <- bounceWrtWall (pi/2 @@ rad)
    s2 <- bounceWrtWall (pi/2 @@ rad)
    s3 <- bounceWrtWall (pi/2 @@ rad)
    s4 <- bounceSpecular
    return s3
