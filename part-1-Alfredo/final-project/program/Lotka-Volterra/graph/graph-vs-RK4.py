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
colores=['#E69F00', '#56B4E9', '#009E73', '#0072B2', '#D55E00', '#CC79A7', '#F0E442']
style_file = 'mine.mplstyle'
style_file = os.path.dirname(__file__)+'/{}'.format(style_file)
plt.style.use(style_file)

#
# Input data
#
fichero_datos = 'out-graph.dat'
fichero_datos = os.path.dirname(__file__)+'/{}'.format(fichero_datos)
with open(fichero_datos, 'r') as file:
    model  = file.readline().strip()
    method = file.readline().strip()
file.close()
t, prey, predator, preyRK, predatorRK, errPrey, errPred = np.loadtxt(fichero_datos, unpack=True, skiprows=3)

#
# Plot
#
fig = plt.figure(figsize=(7.2*2,4.45*1.5))
fig.subplots_adjust(left=.15, bottom=.16, right=.99, top=.97)

fig.suptitle(r'Lotka-Volterra {} model: Foxes ($x$) vs Rabbits ($y$); $x_0 = {:10.0f}$, $y_0 = {:10.0f}$'.format(model,predator[0], prey[0]))

ax11 = fig.add_subplot(2, 3, 1)
ax12 = fig.add_subplot(2, 3, 2)
ax13 = fig.add_subplot(2, 3, (3,6))
ax21 = fig.add_subplot(2, 3, 4, sharex = ax11)
ax22 = fig.add_subplot(2, 3, 5, sharex = ax12)

#
# First plot
#
ax = ax11
ax.plot( t, predatorRK, lw=1.0, color='k',        label='$x$ (RK4)' )
ax.plot( t, predator,           color=colores[0], label='$x$ ({})'.format(method) )

ax.set(
        ylabel=r'Population'
      )

ax.legend(loc='upper left', ncol=1)

ax.set_xlim(min(t), max(t))

#
# Second plot
#
ax = ax12
ax.plot( t, preyRK, lw=1.0, color='k',        label='$y$ (RK4)' )
ax.plot( t, prey,           color=colores[1], label='$y$ ({})'.format(method) )

ax.legend(loc='upper left', ncol=1)

ax.set_xlim(min(t), max(t))

#
# Third plot
#
ax = ax21
ax.plot( t, errPred, color=colores[4], label='$\Delta x/x_{\mathrm{RK4}}$' )

ax.set(
       xlabel=r'$t$ (arbitrary units)',
       ylabel=r'Relative Error vs RK4'
      )

ax.legend(loc='upper left', ncol=1)

#
# Fourth plot
#
ax = ax22
ax.plot( t, errPrey, color=colores[3], label='$\Delta y/y_{\mathrm{RK4}}$' )

ax.set(
       xlabel=r'$t$ (arbitrary units)'
      )

ax.legend(loc='upper left', ncol=1)

#
# Fifth plot
#
ax = ax13
ax.plot( prey, predator, color=colores[2], label=r'$x$ ({})'.format(method) )
ax.plot( preyRK, predatorRK, ls='--', c='k', lw=1.0, label=r'$x$ (RK4)' )
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
