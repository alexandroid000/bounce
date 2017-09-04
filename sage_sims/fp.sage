# playing with symbolic analysis of bouncing robots
# input arbitrary convex polygon as list of XY points in CCW order
# output whether a stable orbit exists
# if so, what range of bounce angles produces it

import itertools
import functools # can't stop the func

var('a,b,c')
#var('x0,x1,x2,x3,x4,y0,y1,y2,y3,y4')
var("x,th,phi,l")
rect = [a,b,a,b]
rect_angs = [pi/2]*4
reg_pent = [a]*5
reg_pent_angs = [3*pi/5]*5
gen_f(x,th,l,phi) = (cos(th)/cos(th-phi))*(l-x)
b0(x) = x

nonreg_pent_pts =   [(10,0)
                    ,(0,7)
                    ,(4,18)
                    ,(16,18)
                    ,(20,7)]
reg_pent_pts = [(0.0, 0.0)
               ,(0.309, 0.951)
               ,(-0.5, 1.5388)
               ,(-1.309, 0.951)
               ,(-1.0, 0.0)]


# take a list of 2d points and extract side lengths and intermediate angles
# assumes closed, simple polygon
def interpret_points(pts):
    triples = zip(pts, pts[1:], pts[2:]) + [(pts[-2], pts[-1], pts[0]), (pts[-1], pts[0],
    pts[1])]
    return map(len_angle, triples)

# helper function for zipped vertex triples
def len_angle(pts):
    (p1, p2, p3) = pts
    v1, v2, v3 = vector(p1), vector(p2), vector(p3)
    dist = (v2-v1).norm()
    side1, side2 = (v2-v1), (v3-v2)
    ang = arccos(side1.dot_product(side2) / (side1.norm() * side2.norm()))
    return (N(dist), N(ang))

# from polygon definition, create sequential-edge bouncing function
def mk_f(pts, theta):
    side_angles = interpret_points(pts)
    bounces = map(lambda sa : gen_f(x, theta, *sa), side_angles)
    return bounces

def comp_gen_f(pts, theta):
    side_angles = interpret_points(pts)
    th_side_angles = [(theta, side, ang) for (side, ang) in side_angles]
    bounce = functools.reduce(  lambda  prev_bounce, args:
                                        gen_f(prev_bounce, *args)
                             ,  th_side_angles
                             ,  b0)
    return bounce



r1(x,th)=gen_f(x,th,a,pi/2)
r2(x,th)=gen_f(x,th,b,pi/2)

# a cycle in a rectangle, side a and b, starting on side a
# assumes the robot strikes every sequential edge
F(x,th)=r2(r1(r2(r1(x,th))))

pent_F(x,th) = gen_f(x,th,a,3*pi/5)

# just find the gdam fixed point
# returns a list
fps = solve( F(x,th) == x, x)

df(x,th) = diff(F(x,th), x)

df_fp(th) = df(fps[0], th)

#solve( -1 < df_fp(th), th)
#solve( df_fp(th) < 1, th)

def plot_iter(f, n, x0):
   mypoints = []
   var('mypoint')
   mypoint = vector([0,x0])
   mypoints.append(mypoint)
   for i in range(1,n):
      newx = N(f(x0))
      mypoint = vector([i,newx])
      mypoints.append(mypoint)
      x0 = newx
   myplot = points(mypoints, rgbcolor=(0.2,0.6, 0.1), pointsize=30) + line(mypoints)
   myplot.show()
   #print(x0)

f(x) = F(x, 1.27).subs(a==2,b==1)

#plot_iter(f, 10, 0.2)
