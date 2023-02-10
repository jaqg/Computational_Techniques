import os
import numpy as np
from matplotlib import pyplot as plt

here = os.getcwd()
files = os.listdir(here)

if not "PyImgs" in files:
	os.mkdir("PyImgs")

poly = lambda x, C: sum([ c * x**i for i, c in enumerate(C) ])

dat_files = [f for f in files if ".dat" in f]

for f in dat_files:
	degree = int(f[4])
	x, y = [], []
	with open(f, 'r') as data:
		for l in data.readlines():
			temp = l.split()
			x.append(float(temp[0]))
			y.append(float(temp[1]))
	X = np.array(x)
	Y = np.array(y)
	regre = np.polyfit(x, y, degree)[::-1]
	nY = np.array([ poly(ix, regre) for ix in x ])
	R = np.corrcoef(Y, nY)
	R2 = R[0,1]**2
	print("-"*60)
	print(f"| Coefficients for {f} using {degree}. degree Polynomial  |")
	print("-"*60)
	print("|" + " "*58 + "|")
	for i, r in enumerate(regre):
		print(f"| c_{i+1} = {r}")
	print("|" + " "*58 + "|")
	print(f"| R^2 = {R2}")
	print("|" + " "*58 + "|")
	print("-"*60)
	Xn = np.linspace(min(x) - 1, max(x) + 1,400)
	Yn = poly(Xn, regre)
	plt.plot(X, Y, "bo")
	plt.plot(Xn, Yn, "r-")
	plt.savefig(os.path.join(here, "PyImgs", f[:-3] + "png"), dpi=300)
	plt.cla()