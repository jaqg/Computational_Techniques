import numpy as np
from matplotlib import pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# Function --------------------------------------------------------------------
f = lambda x, y: np.sin(x + y) + (x - y)**2 - 1.5*x + 3.5*y + 3

X = np.linspace(-3,3,90)
Y = np.linspace(-3,3,90)

X, Y = np.meshgrid(X,Y)

Z = f(X, Y)

# Newton-Raphson --------------------------------------------------------------
x = []
y = []
z = []
with open('output.csv', 'r') as g:
	for l in g.readlines():
		temp = l.split()
		temp = [t for t in temp if t != '']
		if temp[0] != 'X':
			x.append(float(temp[0]))
			y.append(float(temp[1]))
			z.append(float(temp[2]))

# Plot the whole thing --------------------------------------------------------
fig = plt.figure()

for i in range(1,len(x)):
	ax = fig.add_subplot(111, projection='3d')

	ax.plot_surface(X, Y, Z, cmap='viridis', alpha=0.7)
	#ax.plot_wireframe(X, Y, Z, rstride=2, cstride=2)
	#ax.plot(x[:i], y[:i], z[:i], "ro")
	ax.scatter3D(x[:i], y[:i], z[:i], c="red")

	ax.view_init(elev=90., azim=-90)

	plt.savefig(f'iteration_{i}.png', dpi=300)
	plt.cla()