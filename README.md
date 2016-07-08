# bounce

This simulation is built using the
[diagrams](http://projects.haskell.org/diagrams/) library for Haskell. Diagrams
provides a framework for both modelling affine vector spaces and immediately
producing figures from those models.

This library is packaged with
[stack](http://docs.haskellstack.org/en/stable/GUIDE/), so you should be able to
clone the library and run `stack build` successfully if you have stack
installed.

I've implemented a program which computes point-to-point bounce paths with
either random or deterministic angles. For instance, we can bounce at the wall
normal in a star:
![star](https://rawgithub.com/alexandroid000/bounce/det_star.svg)

or bounce randomly in a large polygon:
![large](https://rawgithub.com/alexandroid000/bounce/rand_bigpoly.svg)

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
-   simulate a system where the state is only the set of edges the robot could
    be on
-   Implementing [this paper's algorithm](http://msl.cs.uiuc.edu/~lericks4/papers/icra13bounce.pdf) and finding critical angles as discussed in the conclusion

Suggestions welcome!
