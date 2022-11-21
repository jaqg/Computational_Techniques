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
fig, ax = plt.subplots()
fig.subplots_adjust(left=.15, bottom=.16, right=.99, top=.97)

ax.plot( t, predator, label='$x$ ({})'.format(method) )
ax.plot( t, prey    , label='$y$ ({})'.format(method) )

ax.set(
        title=r'Lotka-Volterra {} model: Foxes ($x$) vs Rabbits ($y$)'.format(model),
        # title=r'Lotka-Volterra model ($x_0 = {x0:.0f},\ y_0 = {y0:.0f}$)'\
       # .format(x0=predator[0], y0=prey[0]),
       xlabel=r'$t$ (arbitrary units)',
       ylabel=r'Population'
      )

ax.legend(loc='upper left')

nombre_grafica = os.path.basename(__file__).replace(".py", ".pdf")
nombre_grafica = nombre_grafica.replace("graph", "{}".format(method))
nombre_grafica = os.path.dirname(__file__)+'/{}'.format(nombre_grafica)
plt.savefig(nombre_grafica, transparent='True', bbox_inches='tight')
