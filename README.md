# bounce

This simulation is built using the
[diagrams](http://projects.haskell.org/diagrams/) library for Haskell. Diagrams
provides a framework for both modelling affine vector spaces and immediately
producing figures from those models, which is handy.

All the fun stuff is in [src/BounceSim.hs](`src/BounceSim.hs`).

This library is packaged with
[stack](http://docs.haskellstack.org/en/stable/GUIDE/), so you should be able to
clone the library and run `stack build` successfully if you have stack
installed.

I've implemented a program which computes point-to-point bounce paths with
either random or deterministic angles. For instance, we can bounce at the wall
normal in a star:

![star](https://cdn.rawgit.com/alexandroid000/bounce/master/det_star.svg)

or bounce randomly in a large polygon:

![large](https://cdn.rawgit.com/alexandroid000/bounce/master/rand_bigpoly.svg)

Scales linearly wrt number of sides of the polygon and number of bounces.

To use, edit `app/Main.hs` and replace the `angs` and `map` variables with the
list of bounce angles and map you want. See `src/Maps.hs` for examples of
maps.

Then, run:

`stack exec bounce-exe -- START NUMBOUNCE -o FILENAME.svg -w PIXWIDTH`

-   `START`: the parameter value on the polygon of where you want to start
    bouncing. Between 0 and 1, need leading zero for floats.
-   `NUMBOUNCE`: integer number of bounces to perform
-   `FILENAME`: output filename
-   `PIXWIDTH`: width of output image in pixels

Upcoming features:

-   choose maps, angle type on command line
-   animated output
-   add the ability to only keep track of the set of edges the robot could be on
    (edge to edge "visibility" graph)
-   Implementing a functional version of [this paper's algorithm](http://msl.cs.uiuc.edu/~lericks4/papers/icra13bounce.pdf) and finding critical angles as discussed in the conclusion

Suggestions welcome!
