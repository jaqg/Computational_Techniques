
# Integration Exercises

Last revision: March, 2022

---

These programs were the last evaluation exercises for the course *TÉCNICAS COMPUTACIONALES Y CÁLCULO NUMÉRICO* in the EMTCCM.

- **Author:** Rony J. Letona
- **email:** [rony.letona@estudiante.uam.es](mailto:rony.letona@estudiante.uam.es)

---

## 1. Running the program

Before doing anything with the programs, please make sure to read the general README at the base of this repository's tree. It should provide details on how to compile all Exercises by using a `make` command.

If that is not possible or if you wish to compile this program as a standalone, please check sections 1. a), 1. b) and 1. c) of the general README, before going into each individual folder and running the following commands:

**Simpson**\
`gfortran -c imethods.f90 -o imethods.o`\
`gfortran -c simpson.f90 -o simpson.o`\
`gfortran simpson.o imethods.o -o simpson.exe`

**Romberg**\
`gfortran -c imethods.f90 -o imethods.o`\
`gfortran -c romberg.f90 -o romberg.o`\
`gfortran romberg.o imethods.o -o romberg.exe`

**Gauss-Legendre**\
`gfortran -c SubGauleg.f90 -o SubGauleg.o`\
`gfortran -c GaussLegendre.f90 -o GaussLegendre.o`\
`gfortran GaussLegendre.o imethods.o -o GaussLegendre.exe`

**IMPORTANT NOTE:**\
The file *SubGauleg.f90* is not provided, as it was provided to me in class and I did not get permission to redestribute. Therefore, I can only say that the code for the module and subroutine can be extracted from the following open source project: [OpenWarp](https://github.com/NREL/OpenWARP)

Link to file: [https://github.com/NREL/OpenWARP/blob/master/source/NemohImproved/Nemoh/Solver/Core/Gaussm3.f90](https://github.com/NREL/OpenWARP/blob/master/source/NemohImproved/Nemoh/Solver/Core/Gaussm3.f90)

Once you have run these commands successfully, you should be able to run each program.

### a) Test

The only way to test if these programs are working is to actually run them. Therefore, I suggest that you try running the following commands:

**Simpson**\
`./simpson.exe`

**Romberg**\
`./romberg.exe`

**Gauss-Legendre**\
`./GaussLegendre.exe`

The programs will greet you and give you a brief summary of what they are going to do. Then, each program will attempt to solve the same problem using a different method.

If the program ran successfully, then you should see the value of the following integral calculated with all 3 numerical integration methods: Simpson's Method, Romberg's Method and the Gauss-Legendre Method.

```math
\int_{1}^{3} \sin{ \left( x^{2} \right) } - \cos{ \left( 2x \right) } dx
```

The result of the integral should be a real number close to $`1.05765068768269`$ for each method.

If this is true, then the program has run successfully.

### b) Inputs

The programs do not require any inputs.

### c) Outputs

If you run the programs as described in section 1. a) of this README, then the output will be displayed on screen.

### d) Theory behind it

It was asked that the programs computed the integral of the function $`f \left( x \right) = \sin{ \left( x^{2} \right) } - \cos{ \left( 2x \right) }`$ in the interval from $`1`$ to $`3`$ using:

- Simpson's Method
```math
\int_{a}^{b} f \left( x \right) dx \approx \frac{\left( b - a \right)}{3 N} \left[ f \left( a \right) + 2 \sum_{i = 0}^{N/2 - 1} f \left( x + 2 i \cdot \frac{\left( b - a \right)}{N} \right) + 4 \sum_{j = 0}^{N/2} f \left( x + (2 j - 1) \cdot \frac{\left( b - a \right)}{N} \right) + f \left( b \right) \right]
```

- Romberg's Method
Considering the following:
```math
h_{n} = \frac{b - a}{2^{n - 1}}
```

For each $`n`$, Richardson extrapolation is applied $`n - 1`$ times to the previous approximation.
```math
\int_{a}^{b} f \left( x \right) dx \approx R \left(n, m\right) = R \left(n, m - 1\right) + \frac{R \left(n, m - 1\right) - R \left(n - 1, m - 1\right)}{4^{m - 1} - 1}
```

It should be noted that the first iterations of $`R`$ can be obtained through the following expressions:
```math
R \left(1, 1\right) = \frac{h_{1}}{2} \left[ f \left( a \right) + f \left( b \right) \right]
```

```math
R \left(n, 1\right) = \frac{1}{2} \left[ R \left(n - 1, 1\right) + h_{n - 1} \sum_{i = 1}^{2^{n - 2}} f \left( a + \left( 2 i - 1 \right) h_{n} \right) \right]
```

- The Gauss-Legendre Method
```math
\int_{-1}^{1} f \left( x \right) dx \approx \sum_{i = 1}^{n} w_{i} f \left( x_{i} \right)
```

Where $`w`$ is a weight computed using *Legendre Polynomials* $`P_{n} \left( x \right)`$ and the following expression.
```math
w_{i} = \frac{2}{\left( 1 - x_{i}^{2} \right) \left[ P'_{n} \left( x \right) \right]^{2}}
```

## 2. FAQs

1. *Can I run the program in Microsoft Windows?* - If you install Cygwin or enable the WSL environment, you should be able to compile and run the program. However, we do suggest that you have a look at section 1. a) of the general README. GNU Fortran is highly recommended to compile the code.

2. *I deleted an important file. What do I do?* - If you deleted a file, please follow this link to locate the project's repository: [https://gitlab.com/zronyj/emtccm-fortran-evaluation-exercises](https://gitlab.com/zronyj/emtccm-fortran-evaluation-exercises)
