---
title: Determining stable trajectories of blind, bouncing robots
author: Alexandra Nilles
documentclass: article
geometry: margin=2cm
bibliography: refs.bib
link-citations: true
csl: ieee.csl
header-includes:
    -   \usepackage{wrapfig}
    -   \usepackage{caption}
    -   \usepackage{jeffe}
    -   \usepackage{amssymb}
    -   \usepackage{graphicx}
...

Abstract
--------

Mobile robots with limited sensing often navigate spaces by travelling in
straight lines until encountering an obstacle. We model such a robot as a point
which can align itself to a certain fixed angle relative to the environment
boundary and drive in straight lines in the free space. Previous work introduced
an algorithm that, given a bounce angle and an environment, identifies parts of
the boundary which will no longer be visited after a bounded number of bounces
or distance travelled. The parts of the environment that the robot could
encounter an unbounded number of times correspond to the *attractor* of the
dynamical system. This work identifies attractors for certain classes of
polygonal environments, as well as provides a correct algorithm generating a
compact sequence of bounce angles which will produce a user-specified trajectory
in an arbitrary environment.

Introduction
------------

Consider the dynamics of a mobile robot, such as a robotic vacuum with a contact
or proximity sensor, as it navigates a room. 

Prior Work
----------

[@bounce]

[@LewOKa13]

Regular Polygons
----------------

As observed in [@bounce], when $\theta$ is $\pi/2-\epsilon$ for small
$\epsilon$, the resulting trajectory is an inscribed equilateral polygon. In
this case, the algorithm described in the previous work does not terminate,
since the distance unbounded region of the boundary is infinitesimally small
(the attractor is one dimensional). It would be useful to identify such cases -
where the attractor is a set of points, not intervals, on $\delta P$.  Then the
algorithm in [@bounce] can be used only for maps with attractors that are
segments on $\delta P$, for which the algorithm is guaranteed to terminate.

### Cycles as Fixed Points of Bounce Map

### Stability of $k$-cycles

### Implications for Uncertainty in Actuation

If a stable orbit is found in a given environment, we can then use the bounds on
the stability derived above to determine a range of angles that will result in
periodic orbits.

Extension to Other Polygons
---------------------------

\begin{wrapfigure}{R}{0.35\textwidth}

\vspace{-2ex}

\includegraphics[width=4cm]{squish.pdf}

\captionof{figure}{Stable limit cycles exist in polygons with fewer
symmetries than regular polygons. \label{squish}}



\end{wrapfigure}




In non-regular polygons, we cannot solve for orbits as the fixed point of one
mapping function. Yet, limit cycles still exist in polygons with enough
symmetry, as seen in Figure \ref{squish}.



Compact Bounce Strategies for Arbitrary Trajectories
----------------------------------------------------

Conclusion and Discussion
-------------------------
