import itertools

var('a,b,c')
var("x,th,phi,l")
rect=[a,b,a,b]
rect_angs=[pi/2,pi/2,pi/2,pi/2]
gen_f(x,th,phi,l)=(cos(th)/cos(th-phi))*(l-x)

# 

def mk_f(f, sides, angs):
    args = zip(sides, angs)
    

r1(x,th)=gen_f(x,th,pi/2,a)
r2(x,th)=gen_f(x,th,pi/2,b)



# a cycle in a rectangle, side a and b, starting on side a
# assumes the robot strikes every sequential edge
F(x,th)=r2(r1(r2(r1(x,th))))

# just find the gdam fixed point
# returns a list
fps = solve( F(x,th) == x, x)

df(x,th) = diff(F(x,th), x)

df_fp(th) = df(fps[0], th)

#solve( -1 < df_fp(th), th)
#solve( df_fp(th) < 1, th)


# the below is the same as "nest(f,n,x)" in sage
#def iter_F(n,x,th):
#    if n==1:
#        return F(x,th)
#    else:
#        return F(iter_F(n-1,x,th), th)
#
#test(x)=F(x,1.27).subs(a==2,b==1)
#
#test(x)

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
