#!/usr/bin/env python
# =====================================================================================
#                    Python script to plot data fitted with LS.exe
# -------------------------------------------------------------------------------------
#
#     Author: Rony J. Letona
#     email:  rony.letona@estudiante.uam.es
#     Date:   March, 2022
#
# =====================================================================================

# Importing the system module to be able to capture command line arguments
import sys

# Checking if a command line argument was given. If not, use the default.
if len(sys.argv) == 4:
	file_name = sys.argv[-3]
	R2 = "R² = " + sys.argv[-2]
	degree = int(sys.argv[-1])
else:
	print("The provided command line arguments are not what was expected.")
	print("Please provide the script with:\n\n")
	print("- name_of_file.csv  :  the name of the file with the data")
	print("- R²                :  the coefficient of determination")
	print("- degree            :  the degree of the polynomial\n")
	print("Quitting ...")
	quit()

# Try importing the plotting module. If it's not possible ... quit.
try:
	from matplotlib import pyplot as plt
except ImportError as e:
	print(e)
	print("Please make sure you have MatPlotLib installed in your computer.")
	quit()

# Try importing the plotting module. If it's not possible ... quit.
try:
	import numpy as np
except ImportError as e:
	print(e)
	print("Please make sure you have NumPy installed in your computer.")
	quit()

# Defining a function for the polynomial
poly = lambda x, C: sum([ c * x**i for i, c in enumerate(C) ])


# Initializing arrays for the old and new values of X and Y
oldX, oldY = [], []
newX, newY = [], []
pyfX, pyfY = [], []

with open(file_name, 'r') as f:         # Open the file with the data
	for l in f.readlines():             # Read the data line by line
		temp = l.split()                # Split the string by spaces
		if temp[0] == "o":              # If the marker is o, ...
			oldX.append(float(temp[1])) # ... store the old X value
			oldY.append(float(temp[2])) # ... store the old Y value
		elif temp[0] == "*":            # If the marker is *, ...
			newX.append(float(temp[1])) # ... store the new X value
			newY.append(float(temp[2])) # ... store the new Y value
		else:                           # In any other case, just quit
			print("The file doen not have the proper format. Please check your data.")
			quit()

oldX = np.array(oldX)
oldY = np.array(oldY)
newX = np.array(newX)
newY = np.array(newY)

regre = np.polyfit(oldX, oldY, degree)[::-1]

fitY = poly(oldX, regre)
R = np.corrcoef(oldY, fitY)
nR2 = R[0,1]**2

pyfX = np.linspace(min(newX), max(newX), 100)
pyfY = poly(pyfX, regre)

plt.title("Data fitted with Rony's LS.exe program written in Fortran")
plt.xlabel("X axis")
plt.ylabel("Y axis")
plt.grid(True)

plt.plot(oldX, oldY, "go", label="Original data")  # Plotting the old values
plt.plot(newX, newY, "r-", label=f"LS Fitted data\n{R2}")    # Plotting the new values
plt.plot(pyfX, pyfY, "b--", label=f"NumPy Fitted data\nR² = {nR2}")
plt.legend()
plt.savefig("new_data.png", dpi=300)               # Saving the plot at 300dpi