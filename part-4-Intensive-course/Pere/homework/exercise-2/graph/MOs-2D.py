# +---------------------------------------------+
# | Author: Jose Antonio Quiñonero Gris         |
# | Creation date: Saturday 15:24:39 05-11-2022 |
# +---------------------------------------------+

#
# Libreries
#
import matplotlib
from matplotlib import markers
from matplotlib.lines import MarkerStyle
import numpy as np
from numpy.core.multiarray import dtype
import pandas as pd
import matplotlib.pyplot as plt
import string
import os
import re
import itertools

matplotlib.use("pgf")

colores=['#56B4E9', '#E69F00', '#009E73', '#0072B2', '#D55E00', '#CC79A7', '#F0E442']
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
datos = open(fichero_datos).readlines()
moleculexyz = datos[1]
moleculexyz = moleculexyz[:len(moleculexyz)-1]
molecule = moleculexyz.removesuffix('.xyz')

#
# Read xyz coordinates
#
fichero_datos = moleculexyz
fichero_datos = os.path.dirname(__file__)+'/../data/{}'.format(fichero_datos)
xcoord, ycoord, zcoord = np.loadtxt(fichero_datos, unpack=True,
                                    skiprows=2, usecols=(1, 2, 3))
atoms = np.genfromtxt(fichero_datos, unpack=True, dtype='str', skip_header=2,
                      usecols=(0))

#
# Read MO coefficients matrix
#
fichero_datos = ''.join([molecule, '-out.dat'])
fichero_datos = os.path.dirname(__file__)+'/../data/{}'.format(fichero_datos)
datos = open(fichero_datos, "r")

beg_match = 'Normalised MO coefficient matrix C'
end_match = '--- Mulliken population analysis ---'
tmp = 1
beg_line = 0
end_line = 0
for line in datos:
    if re.search(beg_match, line):
        beg_line = tmp
    if re.search(end_match, line):
        end_line = tmp - 2
    tmp = tmp + 1

with open(fichero_datos) as fichero:
    coefs = np.loadtxt(itertools.islice(fichero, beg_line, end_line))

#
# Create new arrays with coordinates of the C atoms
#
Catoms = []
Cxcoord = []
Cycoord = []
Czcoord = []
tmp = 0
for atom in atoms:
    if atom == 'C':
        Catoms.append(atom)
        Cxcoord.append(xcoord[tmp])
        Cycoord.append(ycoord[tmp])
        Czcoord.append(zcoord[tmp])

    tmp = tmp + 1

# Number of C atoms
nCatoms = len(Catoms)

#
# Plot
#

fig, axs = plt.subplots(int(nCatoms/6), nCatoms, sharey=True,
                        figsize=(6.7, 6.7/nCatoms*int(nCatoms/6)))
fig.subplots_adjust(left=.15, bottom=.16, right=.99, top=.97)


MO = 0
for ax in axs:
    ax.axis('off')
    ax.plot(Cxcoord, Cycoord, ls='-', lw=1, marker='o', ms=2, c='k')
    ax.plot((Cxcoord[0], Cxcoord[nCatoms-1]),
            (Cycoord[0], Cycoord[nCatoms-1]),
            ls='-', lw=1, marker='o', ms=2, c='k')
    #
    # Plot each MO
    #
    for atom in np.arange(0, nCatoms, 1):
        # Radii of the MO's
        radii = abs(coefs[atom, MO]) * 20
        # Color to represent the phase -> (+) blue; (-) red
        if coefs[atom, MO] < 0:
            MO_color = 'r'
        else:
            MO_color = 'b'
        # Plot the MO's
        ax.plot(Cxcoord[atom], Cycoord[atom],
                marker='o', ms=radii, c=MO_color, alpha=0.5)

    ax.set_ylim(min(Cycoord)-abs(max(Cycoord) - min(Cycoord))/5,
                max(Cycoord)+abs(max(Cycoord) - min(Cycoord))/5)

    MO = MO + 1

# fig.suptitle()

#
# First plot
#
# ax=axs[0]
#
# ax.hlines()
# ax.plot()
# ax.text(x, y, fontsize=14, horizontalalignment='center', verticalalignment='bottom')
#
# ax.ticklabel_format(axis='y', style='sci', scilimits=(2,2))
# ax.ticklabel_format(useOffset=False)
# ax.locator_params(axis='x', nbins=4)
#
# ax.set_xlim(0.5,3.5)
# ax.set_ylim(-50,40)
#
# ax.set(
#         title=r'Energy, $\Delta E$',
#        # xlabel=r'Scan coordinate ($\mathrm{\AA}$)',
#        ylabel=r'Energy, $\Delta E$ ($\mathrm{kcal/mol}$)'
#       )
#
# ax.set_xticklabels(['','Reactants','TS','Products',''], fontsize=18)
#
# ax.legend()
#
# ax.grid(False)

# Enumerate the subplots
# for n, ax in enumerate(axs):
#     ax.text(-0.1, 1.1, string.ascii_uppercase[n], transform=ax.transAxes,
#             size=20, weight='bold')

nombre_grafica = ''.join([molecule, '.pdf'])
plt.savefig(nombre_grafica, transparent='True', bbox_inches='tight')
#
nombre_grafica = ''.join([molecule, '.pgf'])
plt.savefig(nombre_grafica, format='pgf')
