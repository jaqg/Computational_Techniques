import numpy as np
from matplotlib import pyplot as plt

poly = lambda x, C: sum([ c * x**i for i, c in enumerate(C) ])

x, y = [], []
with open('data.dat', 'r') as f:
	for l in f.readlines():
		temp = l.split()
		x.append(float(temp[0]))
		y.append(float(temp[1]))

n = int(input("Degree: "))

print("-"*60)

print("Polynomial matrix: ")

X = np.array([[ix**i for i in range(n+1)] for ix in x])

print(X)

print("-"*60)

print("Projection (augmented) matrix: ")

B = np.matmul(np.transpose(X), X)

for r in B:
	for c in r:
		print(f"{c}\t", end="")
	print("")

print("-"*60)

print("Projected y vector: ")

z = np.matmul(np.transpose(X), np.array(y))

print(z)

print("-"*60)

print("Inverted projection (augmented) matrix: ")

iB = np.linalg.inv(B)

for r in iB:
	for c in r:
		print(f"{c}\t", end="")
	print("")

print("-"*60)

print("Coefficients: ")

c = np.matmul(iB, z)

for i, r in enumerate(c):
	print(f"c_{i+1} = {r}")

print("-"*60)

newX = np.linspace(min(x)-0.5, max(x)+0.5, 200)
newY = np.array([ poly(ix, c) for ix in newX ])

R = np.corrcoef(y, [ poly(ix, c) for ix in x ])
R2 = R[0,1]**2

print(f"R^2 = {R2}")

plt.plot(x, y, "bo")
plt.plot(newX, newY, "r-")
plt.show()