---
title: Proofs about behavior of blind bouncing robots
author: Alli Nilles
geometry: margin=2cm
...

Given a closed polygonal region $P$ with boundary $\partial P$. There is a map
$B_\theta : \partial P \to \partial P$ where a robot that is on the boundary at
$p$ will next impact the boundary at $B_\theta(p)$.  The map is completely
determined by the outgoing angle with respect to the wall, $\theta :
0 < \theta < \pi$.

Bounces Between Parallel Lines enter 2-cycles immediately
---------------------------------------------------------

See Erickson, LaValle et al. Note that the convention for $\theta$ is different
in this work: our notation is more consistent with the standard in billiards.

Bounces between "Codependent Segments" go monotonically outward from vertex
---------------------------------------------------------------------------

See Erickson, LaValle et al

Bounces in regular polygon converge to stable orbit for certain angles
----------------------------------------------------------------------

Let $P$ be an equilateral triangle, with side length $a$. Let the vertices of
the triangle be labelled $A, B, C$ in counterclockwise ordering.

Start the robot at point $p_0$ on segment $\overline{AB}$ which has distance $x$
from vertex $A$.

Let $f(x,\theta)$ be a function that returns the distance from vertex $B$ of the
point $B_\theta(p_0)$. This assumes that point $B_\theta(p_0)$ is on segment
$\overline{BC}$, but this is symmetric to the other case where the bounce goes
to segment $\overline{CA}$.

The points $p_0, B_\theta(p_0)$, and $B$ form a triangle. The angle $\angle p_0
B B_\theta (p_0)$ is $\pi/3$ since this is an equilateral triangle. The angle
$\angle B_\theta (p_0) p_0 B$ is $\pi/2 - \theta$, and the segment
$\overline{p_0 B}$ has length $a-x$. Thus we can use the law of sines to solve
for $f(x, \theta)$:

$$ \frac{f(x,\theta)}{sin(\pi/2 - \theta)} = \frac{a-x}{sin(\pi - \pi/3 - (\pi/2 - \theta))} $$

$$ f(x, \theta) = \frac{(a-x) cos(\theta)}{sin(\pi/6 + \theta)} $$

Let $c(\theta) = \frac{cos(\theta)}{sin(\pi/6 + \theta)}$. Fix $\theta$ and let
the infinite iteration of $f(f(...(x))) = F$. Then:

$F = a \sum_{i=1}^{\infty} (-1)^{i+1} c^i$

which converges when $| c | < 1$. This means that $f(x,\theta)$ has a fixed
point when $c(\theta)$ is between -1 and 1.


