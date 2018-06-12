import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from math import pi

# Create the figure
fig = plt.figure()
# Add an axes
ax = fig.add_subplot(111,projection='3d')
# Tweaking display region and labels
ax.set_xlim(0, 3.2)
ax.set_ylim(3.2, 0)
ax.set_zlim(0, 3.2)
ax.set_xlabel('alpha')
ax.set_ylabel('beta')
ax.set_zlabel('theta')

x = np.linspace(0, pi, 1000)
print(x)
y = np.linspace(0, pi, 1000)


z = (pi - x - y)

z = z[z >= 0]


# plot the surface
ax.plot_surface(x, y, z, alpha=0.2)

plt.show()
