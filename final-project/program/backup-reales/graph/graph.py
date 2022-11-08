# +---------------------------------------------+
# | Author: Jose Antonio Quiñonero Gris         |
# | Creation date: Saturday 15:24:39 05-11-2022 |
# +---------------------------------------------+

# Librerias
import numpy as np
import matplotlib.pyplot as plt
import os

style_file = 'mine.mplstyle'
style_file = os.path.dirname(__file__)+'/{}'.format(style_file)
plt.style.use(style_file)

# *****************************************************************************
# INICIO
# *****************************************************************************

fichero_datos = 'out-graph.dat'
fichero_datos = os.path.dirname(__file__)+'/{}'.format(fichero_datos)
t, prey, predator = np.loadtxt(fichero_datos, unpack=True, skiprows=1)

# Graph parameters
#Default width x height: 7.2x4.45
width = 7.2
height = width*(4.45/7.2)

fig, ax = plt.subplots(figsize=(width, height))
fig.subplots_adjust(left=.15, bottom=.16, right=.99, top=.97)

# Graph limits
xmin = 0
xmax = max(t)
ymin = 0
if max(prey)>max(predator):
    ymax = max(prey)+max(prey)/10
else:
    ymax = max(predator)+max(predator)/10

ax.plot(t,predator,label='Foxes, $x$')
ax.plot(t,prey,label='Rabbits, $y$')

ax.set(
        title=r'Lotka-Volterra model ($x_0 = {x0:.0f},\ y_0 = {y0:.0f}$)'\
       .format(x0=predator[0], y0=prey[0]),
       xlabel=r'$t$ (arbitrary units)',
       ylabel=r'Population'
      )

ax.set_xlim(xmin,xmax)
ax.set_ylim(ymin,ymax)

ax.legend(loc='best')

# ax.text(0.02, 0.975, 'a)', fontsize=14, bbox=dict(facecolor='white', alpha=0.5), horizontalalignment='left', verticalalignment='top', transform=ax.transAxes)

nombre_grafica = os.path.basename(__file__).replace(".py", ".pdf")
nombre_grafica = os.path.dirname(__file__)+'/{}'.format(nombre_grafica)
plt.savefig(nombre_grafica, transparent='True', bbox_inches='tight')
