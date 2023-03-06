# +---------------------------------------------+
# | Author: Jose Antonio Quiñonero Gris         |
# | Creation date: Saturday 15:24:39 05-11-2022 |
# +---------------------------------------------+

#
# Libreries
#
import matplotlib
from matplotlib.lines import MarkerStyle
import numpy as np
import matplotlib.pyplot as plt
import string
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
# Exercise label for the name of the plot
#
fichero_datos = 'input.dat'
fichero_datos = os.path.dirname(__file__)+'/../data/{}'.format(fichero_datos)
exercise_label = open(fichero_datos).readlines()
exercise_label = exercise_label[3].removeprefix('./data/')

#
# (x, y) coordinates
#
fichero_datos = 'xy-graph.dat'
fichero_datos = os.path.dirname(__file__)+'/../data/{}'.format(fichero_datos)
xdata, ydata = np.loadtxt(fichero_datos, unpack=True, skiprows=1)

#
# Coefficients
#
fichero_datos = 'coeffs-graph.dat'
fichero_datos = os.path.dirname(__file__)+'/../data/{}'.format(fichero_datos)
grad, coeffs = np.loadtxt(fichero_datos, delimiter=';', unpack=True,
                          skiprows=1)

#
# Fitted polynomial
#
def f(igrads, icoeffs, x):
    res = 0
    for i in igrads:
        res = res + icoeffs[int(i)] * x**i
    return res

#
# Plot
#
fig, ax = plt.subplots()

x = np.arange(min(xdata), max(xdata), abs(max(xdata)-min(xdata))/500)

ax.plot(xdata, ydata, ls='None', label=r'Data points')
ax.plot(x, f(grad, coeffs, x), marker='None', label=r'$f(x)$')

ax.set(
        title=r' '.join(['Data file:', exercise_label]),
        xlabel=r'$x$',
        ylabel=r'$y$'
        )

ax.set_xlim(min(xdata)-abs(max(xdata)-min(xdata))/10,
            max(xdata)+abs(max(xdata)-min(xdata))/10)
ax.set_ylim(min(ydata)-abs(max(ydata)-min(ydata))/10,
            max(ydata)+abs(max(ydata)-min(ydata))/10)

ax.legend()

nombre_grafica = exercise_label.replace('.dat', '-plot.pdf')
nombre_grafica = ''.join(['graph/', nombre_grafica])
nombre_grafica = nombre_grafica.removesuffix('\n')
plt.savefig(nombre_grafica, transparent='True', format='pdf',
            bbox_inches='tight')
#
nombre_grafica = exercise_label.replace('.dat', '-plot.pgf')
nombre_grafica = ''.join(['graph/', nombre_grafica])
nombre_grafica = nombre_grafica.removesuffix('\n')
plt.savefig(nombre_grafica, format='pgf')
