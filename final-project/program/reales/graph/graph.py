# +---------------------------------------------+
# | Author: Jose Antonio Quiñonero Gris         |
# | Creation date: Saturday 15:24:39 05-11-2022 |
# +---------------------------------------------+

#
# Libreries
#
import numpy as np
import matplotlib.pyplot as plt
import os

#
# Style
#
style_file = 'mine.mplstyle'
style_file = os.path.dirname(__file__)+'/{}'.format(style_file)
plt.style.use(style_file)

#
# Input data
#
fichero_datos = 'out-graph.dat'
fichero_datos = os.path.dirname(__file__)+'/{}'.format(fichero_datos)
with open(fichero_datos, 'r') as file:
    model = file.readline().strip()
    method = file.readline().strip()
file.close()
t, prey, predator, preyRK, predatorRK, errPrey, errPred = np.loadtxt(fichero_datos, unpack=True, skiprows=3)

#
# Plot
#
fig, axs = plt.subplots(1,2, figsize=(7.2*1.5, 4.45))
fig.subplots_adjust(left=.15, bottom=.16, right=.99, top=.97)

fig.suptitle(r'Lotka-Volterra {} model: Foxes ($x$) vs Rabbits ($y$); $x_0 = {:10.0f}$, $y_0 = {:10.0f}$'.format(model,predator[0], prey[0]))

#
# First plot
#
ax = axs[0]
ax.plot( t, predator, label='$x$ ({})'.format(method) )
ax.plot( t, prey    , label='$y$ ({})'.format(method) )

ax.set(
       xlabel=r'$t$ (arbitrary units)',
       ylabel=r'Population'
      )

ax.legend(loc='upper left')

#
# Second plot
#
ax = axs[1]
ax.plot( prey, predator )
ax.scatter(prey[0], predator[0], marker='o', s=max(prey)+max(predator), color='r', label=r'$\left( x_0, y_0\right)$', zorder=2)

ax.set(
       xlabel=r'Prey',
       ylabel=r'Predator'
      )

ax.legend()

nombre_grafica = os.path.basename(__file__).replace(".py", ".pdf")
nombre_grafica = nombre_grafica.replace("graph", "{}-{}".format(model,method))
nombre_grafica = os.path.dirname(__file__)+'/plots/{}'.format(nombre_grafica)
plt.savefig(nombre_grafica, transparent='True', bbox_inches='tight')
