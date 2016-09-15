# bounce

This is a simulator and analysis engine for [bouncing
robots](http://msl.cs.uiuc.edu/~lericks4/papers/icra13bounce.pdf), a dynamical
system similar to pinball billiards. This simulation is built using the
[diagrams](http://projects.haskell.org/diagrams/) library for Haskell. Diagrams
provides a framework for both modelling affine vector spaces and immediately
producing figures from those models, which is handy. This library is packaged
with [stack](http://docs.haskellstack.org/en/stable/GUIDE/).

Right now we can compute point-to-point bounce paths with either random or
deterministic angles. For instance, we can bounce at the wall normal in a star:

![star](https://cdn.rawgit.com/alexandroid000/bounce/master/examples/det_star.svg)

or bounce randomly in a large polygon:

![large](https://cdn.rawgit.com/alexandroid000/bounce/master/examples/rand_bigpoly.svg)

There is also functionality for making animated gifs and for producing some of
the usual plots for analysis of dynamical systems. See below for more
documentation and examples.

# Installation

-   Install stack, [guide here](http://docs.haskellstack.org/en/stable/install_and_upgrade/)
-   Clone repo
-   `cd` into `bounce` and run `stack init`
-   Run `stack build` and wait while it builds all the dependencies.

# Usage

### Static Bounce Simulations

Usage: bounce-exe (-t|--write-to FILENAME) (-n|--num NUM_BOUNCES)
                  (-a|--angle ANGLE) (-s|--start START_PARAM) [-r|--random]

-   `FILENAME`: output filename, svg format
-   `NUM_BOUNCES`: integer number of bounces to perform
-   `ANGLE`: Floating point number, in radians, of angle to bounce at. Needs
    leading 0 if the number is less than 1. Optional, default 0.2 rad
-   `START`: the parameter value (in the interval [0,1]) on the polygon of where
    you want to start bouncing. Be sure to include the leading zero for
    parameters like 0.5.
-   `-r` flag is optional and will create a random bounce angle at every bounce

**To change map:**

Edit `app/Main.hs` and replace the `simMap` variable at the beginning of the
file with the map you want. See `src/Maps.hs` for examples of maps. `Maps` is
imported into Main so you can use any of those maps or add your own.

If you edit `Main.hs` you'll need to run `stack build` again in the top level
directory to recompile.

### Animated Bounce Simulations

-   Edit `app/Animate.hs` with the angle, map, start parameter, and number of
    bounces that you want
-   run `stack build` in the top level directory
-   run `stack exec mkGif -- -o FILENAME.gif -w PIXWIDTH`

### Generating Dynamical Systems Analysis Plots

-   Run `stack ghci` in the `bounce` directory
-   If you get a message about which main module to use, hit enter and ignore it
-   The function `mkChart` is what generates the plots. It has many parameters,
    all required. An example is below.

```ghci
λ> mkChart (pts2poly Maps.star) [pi/4,pi/2] 0.2 5000 "cobweb" "cobweb.svg"
```
-   *Map:* The first argument is the polygon map, created with the helper function
`pts2poly` from any of the maps specified in `src/Maps.hs`.
-   *Angles:* The second argument is a list of bounce angles to generate plots for (data
    will be overlaid on one plot, in different colors w/ legends). If doing just
    one angle, you still need to wrap in in the list syntax [].
-   *Start Parameter:* The third argument is the parameter on the polygon to
    start bouncing (used by "return" and "cobweb" plots, see below)
-   *Number of bounces:* Fourth parameter is an integer number of bounces to do,
    used by return and cobweb plots.
-   *Plot type:* Currently the following plot types are supported:
    -   "scan": Scatter plot of $x_n$ vs $x_{n+1}$ for 20 values of $x_n$
        between 0 and 1
    -   "return": Plots the [return
        map](https://en.wikipedia.org/wiki/Poincaré_plot) starting at the
        specified parameter. Connects successive bounce points with lines.
    -   "cobweb": Makes a [cobweb
        plot](https://en.wikipedia.org/wiki/Cobweb_plot) starting at the
        specified parameter (TODO: animate as a function of bounce angle like in
        that wiki article).
-   *Filename*: desired filename. Will save relative to directory where `stack
    ghci` is running.

Example cobweb plot, generated from the above example:

![large](https://cdn.rawgit.com/alexandroid000/bounce/master/examples/cobweb.svg)


Upcoming features:

-   choose maps on command line
-   add the ability to only keep track of the set of edges the robot could be on
    (edge to edge "visibility" graph)
-   Implementing a functional version of [this paper's
    algorithm](http://msl.cs.uiuc.edu/~lericks4/papers/icra13bounce.pdf) and
    finding critical angles as discussed in the conclusion

Suggestions welcome!
