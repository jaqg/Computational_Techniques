# +---------------------------------------------+
# | Author: Jose Antonio Quiñonero Gris         |
# | Creation date: Saturday 15:24:39 05-11-2022 |
# +---------------------------------------------+

#
# Libreries
#
import matplotlib
import numpy as np
import matplotlib.pyplot as plt
import os

matplotlib.use("pgf")

#
# Style
#
style_file = 'mine.mplstyle'
style_file = os.path.dirname(__file__)+'/{}'.format(style_file)
plt.style.use(style_file)

#
# Input data
#
fichero_datos = 'integration.dat'
fichero_datos = os.path.dirname(__file__)+'/{}'.format(fichero_datos)
iter, simpson, romberg, gauss = np.loadtxt(fichero_datos, unpack=True,
                                           skiprows=1)

#
# Plot
#
fig, ax = plt.subplots()

ax.plot(iter[0:8], simpson[0:8], label=r'Simpson')
ax.plot(iter, romberg, label=r'Romberg')
ax.plot(iter[0:9], gauss[0:9], label=r'Gauss-Legendre')

ax.set(
        # title=r'Energy, $\Delta E$',
        xlabel=r'Iteration step',
        ylabel=r'Value of the integral'
        )
#
ax.legend()
#

nombre_grafica = os.path.basename(__file__).replace(".py", ".pdf")
plt.savefig(nombre_grafica, transparent='True', bbox_inches='tight')
#
nombre_grafica_2 = os.path.basename(__file__).replace(".py", ".pgf")
plt.savefig(nombre_grafica_2, format='pgf')
