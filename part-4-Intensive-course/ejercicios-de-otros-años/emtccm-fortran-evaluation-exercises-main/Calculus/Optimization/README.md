
# Optimization Exercises

Last revision: March, 2022

---

These programs were the last evaluation exercises for the course *TÉCNICAS COMPUTACIONALES Y CÁLCULO NUMÉRICO* in the EMTCCM.

- **Author:** Rony J. Letona
- **email:** [rony.letona@estudiante.uam.es](mailto:rony.letona@estudiante.uam.es)

---

## 1. Running the program

Before doing anything with the programs, please make sure to read the general README at the base of this repository's tree. It should provide details on how to compile all Exercises by using a `make` command.

If that is not possible or if you wish to compile this program as a standalone, please check sections 1. a), 1. b) and 1. c) of the general README, before going into each individual folder and running the following commands:

**Steepest Descent**\
`gfortran -c dmethods.f90 -o dmethods.o`\
`gfortran -c SD_2D.f90 -o SD_2D.o`\
`gfortran SD_2D.o dmethods.o -o SD_2D.exe`

**Newton-Raphson**\
`gfortran -c dmethods.f90 -o dmethods.o`\
`gfortran -c NR_2D.f90 -o NR_2D.o`\
`gfortran NR_2D.o dmethods.o -o NR_2D.exe`

Once you have run this command successfully, you should be able to run the program.

### a) Test

The only way to test if these programs are working is to actually run them. Therefore, I suggest that you try running the following commands:

**Steepest Descent**\
`./SD_2D.exe`

**Newton-Raphson**\
`./NR_2D.exe`

The programs will greet you and give you a brief summary of what they are going to do. Then, each program will attempt to solve the same problem using a different method.

If the program ran successfully, then you should see the value of the coordinates for the local minimum of the following function computed with both the Steepest Descent and the Newton-Raphson methods.

```math
g \left( x, y \right) = \sin{ \left( x + y \right) + \left( x - y \right)^{2} - 1.5 x + 3.5 y + 3 }
```

The the local minimim should lie around the following coordinates $`\left( 2.1958059946557387, 0.94580599424803535 \right)`$ for both methods.

If this is true, then the program has run successfully.

**IMPORTANT NOTE:**\
Please keep in mind that the Steepest Descent will not converge to the minimum. The program will stop after 50 iterations.

### b) Inputs

The programs do not require any inputs.

### c) Outputs

If you run the programs as described in section 1. a) of this README, then the required output will be displayed on screen.

However, both methods will produce a single output file named `output.csv` The file should have a title row with a the name of each column. Then, the data of each iteration should be shown on each subsequent row.

### d) Theory behind it

It was asked that the programs found the local minumim of the function $`G \left( \hat{q} \right) = g \left( x, y \right) = \sin{ \left( x + y \right) + \left( x - y \right)^{2} - 1.5 x + 3.5 y + 3 }`$ using:

- The Steepest Descent Method
```math
\hat{q}_{i + 1} = \hat{q}_{i} - \gamma \nabla G \left( \hat{q}_{i} \right)
```

- The Newton-Raphson Method
```math
\hat{q}_{i + 1} = \hat{q}_{i} - \left[ H \left( \hat{q}_{i} \right) \right]^{-1} \nabla G \left( \hat{q}_{i} \right)
```

Where $`H \left( \hat{q}_{i} \right)`$ is the *Hessian* of $`G`$ evaluated at $`\hat{q}_{i}`$.

For both methods, the derivatives are computed numerically using the following expressions:

- First (Partial) Derivative
```math
\dfrac{\partial}{\partial \hat{q}_{i}} F \left( \hat{q} \right) = \frac{ F \left( \hat{q}_{1}, .., \hat{q}_{i} + \delta, .., \hat{q}_{n} \right) - F \left( \hat{q}_{1}, .., \hat{q}_{i} - \delta, .., \hat{q}_{n} \right) }{2 \delta}
```

- Second (Partial) Derivative
```math
\dfrac{\partial^{2}}{\partial \hat{q}_{i} \partial \hat{q}_{j}} F \left( \hat{q} \right) = \frac{ F \left( \hat{q}_{1}, .., \hat{q}_{i} + \delta, \hat{q}_{j} + \delta, .., \hat{q}_{n} \right) - F \left( \hat{q}_{1}, .., \hat{q}_{i} - \delta, \hat{q}_{j} + \delta, .., \hat{q}_{n} \right) - F \left( \hat{q}_{1}, .., \hat{q}_{i} + \delta, \hat{q}_{j} - \delta, .., \hat{q}_{n} \right) + F \left( \hat{q}_{1}, .., \hat{q}_{i} - \delta, \hat{q}_{j} - \delta, .., \hat{q}_{n} \right) }{4 \delta^{2}}
```

For the derivative to give the best results, $`\delta`$ was set to be equal to $`10^{-6}`$ using **double precision** so that the error was minimized.

### e) Plots
To obtain plots of how the programs have iterated over the surface described by the function, a Python script has been provided in each folder. When running it, after having obtained the `output.csv` file, several plots will be generated in high resolution: one for each step in the algorithm. To run the script, just run the commands in their respective folder as follows:

- Steepest Descent method: `python SD_plotter.py`
- Newton-Raphson method: `python NR_plotter.py`

Please keep in mind that, to run these scripts, you will have to have **NumPy** and **MatPlotLib** installed in your computer.

## 2. FAQs

1. *Can I run the program in Microsoft Windows?* - If you install Cygwin or enable the WSL environment, you should be able to compile and run the program. However, I do suggest that you have a look at section 1. a) of the general README. GNU Fortran is highly recommended to compile the code.

2. *I deleted an important file. What do I do?* - If you deleted a file, please follow this link to locate the project's repository: [https://gitlab.com/zronyj/emtccm-fortran-evaluation-exercises](https://gitlab.com/zronyj/emtccm-fortran-evaluation-exercises)