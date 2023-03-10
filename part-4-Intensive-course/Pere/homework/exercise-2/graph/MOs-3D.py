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
from numpy.core.multiarray import arange, dtype
import pandas as pd
import matplotlib.pyplot as plt
import string
import os
import re
import itertools

# matplotlib.use("pgf")

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
fig = plt.figure(figsize=plt.figaspect(0.5))
# fig, axs = plt.subplots(int(nCatoms/6), nCatoms, sharey=True,
#                         figsize=(6.7, 6.7/nCatoms*int(nCatoms/6)))
# fig.subplots_adjust(left=.15, bottom=.16, right=.99, top=.97)

def plt_MO(center, radius, thecolor, thealpha):
    # draw sphere
    u, v = np.mgrid[0:2*np.pi:50j, 0:np.pi:50j]
    x = radius*np.cos(u)*np.sin(v)
    y = radius*np.sin(u)*np.sin(v)
    z = radius*np.cos(v)
    ax.plot_surface(x-center[0], y-center[1], z-center[2],
                    color=thecolor, alpha=thealpha)

MO = 0
MO_alpha = 0.5
for subpl in np.arange(1, nCatoms+1, 1):
    ax = fig.add_subplot(int(nCatoms/6), nCatoms, subpl, projection='3d')
    ax.axis('off')
    ax.plot3D(Cxcoord, Cycoord, Czcoord, ls='-', lw=1, marker='o', ms=2, c='k')
    ax.plot3D((Cxcoord[0], Cxcoord[nCatoms-1]),
              (Cycoord[0], Cycoord[nCatoms-1]),
              (Czcoord[0], Czcoord[nCatoms-1]),
              ls='-', lw=1, marker='o', ms=2, c='k')
    #
    # Plot each MO
    #
    for atom in np.arange(0, nCatoms, 1):
        # Radii of the MO's
        radii = abs(coefs[atom, MO]) * 20
        # Color to represent the phase -> (+) blue; (-) red
        if coefs[atom, MO] < 0:
            MO_color = '#ed000d7b'
        else:
            MO_color = '#0082ed6b'
        # Plot the MO's
        ax.plot3D(Cxcoord[atom], Cycoord[atom], Czcoord[atom],
                  marker='o', ms=radii, c=MO_color, alpha=MO_alpha)
        # plt_MO([Cxcoord[atom], Cycoord[atom], Czcoord[atom]],
        #        radii/20, MO_color, MO_alpha)
        #
        ax.view_init(elev=50., azim=20)
        ax.dist = 10
        #
    ax.set_ylim(min(Cycoord)-abs(max(Cycoord) - min(Cycoord))/5,
                max(Cycoord)+abs(max(Cycoord) - min(Cycoord))/5)
    MO = MO + 1

plt.show()

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
