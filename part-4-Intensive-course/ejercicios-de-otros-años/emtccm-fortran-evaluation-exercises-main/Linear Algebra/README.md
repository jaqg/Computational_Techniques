
# Linear Algebra Exercises

Last revision: March, 2022

---

These programs were the last evaluation exercises for the course *TÉCNICAS COMPUTACIONALES Y CÁLCULO NUMÉRICO* in the EMTCCM.

- **Author:** Rony J. Letona
- **email:** [rony.letona@estudiante.uam.es](mailto:rony.letona@estudiante.uam.es)

---

## 1. Running the programs

Before doing anything with the programs, please make sure to read the general README at the base of this repository's tree. It should provide details on how to compile all Exercises by using a `make` command.

If that is not possible or if you wish to compile this program as a standalone, please check sections 1. a), 1. b) and 1. c) of the general README, before going into each individual folder and running the following commands:

**Least Squares**\
`gfortran -c iomethods.f90 -o iomethods.o`\
`gfortran -c lamethods.f90 -o lamethods.o`\
`gfortran -c LS.f90 -o LS.o`\
`gfortran LS.o iomethods.o lamethods.f90 -o LS.exe`

**Huckel**\
`gfortran -c iomethods.f90 -o iomethods.o`\
`gfortran -c lamethods.f90 -o lamethods.o`\
`gfortran -c huckel.f90 -o huckel.o`\
`gfortran huckel.o iomethods.o lamethods.f90 -o huckel.exe`

Once you have run these commands successfully, you should be able to run each program.

### a) Test

The only way to test if these programs are working is to actually run them. However, these programs can not only run un *interactive mode*, but also (if you are in a hurry) in *command line* mode.

#### i. Interactive Mode

I suggest that you try running the following commands:

**Least Squares**\
`./LS.exe`

**Huckel**\
`./huckel.exe`

The programs will greet you and give you a brief summary of what they are going to do. Then, each program will request data from you. Please keep in mind that if the program asks you for a file, you have to provide the **complete file name** with the extension, and the file **HAS** to be in the same folder as the program.

If the program ran successfully, then you should see an output of the required calculation and some new files in your folder.

#### ii. Command Line Mode

Please consider the following:

**Least Squares**\
To run this program in command line mode, please run this command:

`./LS.exe name_of_file.dat degree`

Where `name_of_file.dat` is the name of the data file provided to the program, and `degree` is an integer specifying the degree of the polynomial function to use in order to fit the data.

**Huckel**\
To run this program in command line mode, please run this command:

`./huckel.exe name_of_file.xyz charge alpha beta`

Where `name_of_file.xyz` is the name of the molecule file provided to the program in **XYZ** format, `charge` is an integer specifying the total charge of the molecule, `alpha` is the first energy parameter, and `beta` is the second energy parameter. Typically, the last two take values of $`-11.4`$ and $`-0.8`$ respectively.

If the program ran successfully, then you should see an output of the required calculation and some new files in your folder.

### b) Inputs

Each program requires a file with data and some parameters which can be fed to the program in interactive mode or in command line mode. Make sure that:

- **Least Squares:** it uses a file and a single parameter
	1. *File:* A data file whith two columns. The first column are X values and the second column are Y values.
	2. *Parameter:* An integer value specifying the degree of the polynomial function to be used, to fit the data.

- **Huckel:** it uses a file and 3 parameters
	1. *File:* An XYZ molecular coordinates file, according to the following format: [XYZ (format)](https://openbabel.org/wiki/XYZ_(format))
	2. *First parameter:* An integer value specifying the charge of the molecule.
	3. *Second parameter:* A real value specifying the first energy parameter of the Huckel method: $`\alpha`$. Typically, this value is the following: $`\alpha = -11.4`$
	4. *Third parameter:* A real value specifying the second energy parameter of the Huckel method: $`\beta`$. Typically, this value is the following: $`\beta = -0.8`$

### c) Outputs

If you run the programs as described in section 1. a) of this README, then the output will be displayed on screen, and some files should have appeared in the same folder as your program.

**Least Squares**\
In this case, the program will show you the coefficient of determination $`R^{2}`$, the polynomial's coefficients, the values of $`x`$, and the old and new (fitted) values of $`y`$.

Additionally, the following files should have been created:

- `coeffs.csv`: the polynomial's coefficients
- `new_data.csv`: the old values of $`x`$ and $`y`$ marked with an `o`, and many new values of $`x`$ and $`y`$ marked with a `*`.
- `new_data.png`: an image with the the old data, new data and a new fit using NumPy.

If there is no image (a plot wasn't created), please try the following. To plot the old data, new data and perform a new fit using NumPy, please run `plotter.py` in the following way:

`python plotter.py new_data.csv coefDet degree`

Where `plotter.py` is the name of the script, `new_data.csv` the output file described above, `coefDet` is the coefficient of determination shown in `LS.exe`'s on-screen output, and `degree` is an integer specifying the degree of the polynomial function to use in order to fit the data.

If this doesn't work, please check your Python installation; you may refer to the main README.

**Huckel**\
In this other case, the program will show you the final $`\pi`$-orbital energy, the Huckel (the eigenstates of the $`\pi`$ system) and Mulliken analyses (Mulliken population and Bond Order):

- Huckel:
	1. The number of the eigenstate
	2. The eigenvalue of that state
	3. The energy of that eigenstate
	4. The occupancy of the particular orbital
	5. A matrix with the eigenvectors for each eigenstate

- Mulliken:
	1. The number of the atom
	2. The Mulliken population
	3. A matrix with the bond order between pairs of atoms

Additionally, some files should have been created. For this case, I'll assume that the name of the molecule is **molecule**. This means that the XYZ file should have been named `molecule.xyz`. That being the case, the output files will be:

- `molecule_huckel.dat`: the eigenvalue, energy and occupancy of each eigenstate in columns
- `molecule_eigenvectors.dat`: a matrix with the eigenvectors of each eigenstate in columns
- `molecule_mulliken.dat`: a matrix with the bond orders of each pair of atoms. Please note that the diagonal has the Mulliken population values for each atom.

In order to plot the electron density of each orbital, please run `density_plotter.py` in the following way:

`python density_plotter.py molecule`

Please note that both the `molecule.xyz` and the `molecule_eigenvectors.dat` files should be in the same folder as `density_plotter.py`. Otherwise, this will not work. Also, do not try to plot electron densities of 3-dimensional molecules; `density_plotter.py` only works in 2D. Finally, please consider that the plots will be in high definition; please have some patience, for some of them will take a while.

## 2. FAQs

1. *Can I run the program in Microsoft Windows?* - If you install Cygwin or enable the WSL environment, you should be able to compile and run the program. However, we do suggest that you have a look at section 1. a) of the general README. GNU Fortran is highly recommended to compile the code.

2. *I deleted an important file. What do I do?* - If you deleted a file, please follow this link to locate the project's repository: [https://gitlab.com/zronyj/emtccm-fortran-evaluation-exercises](https://gitlab.com/zronyj/emtccm-fortran-evaluation-exercises)
