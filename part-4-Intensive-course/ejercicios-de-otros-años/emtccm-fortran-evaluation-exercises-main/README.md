
# EMTCCM Fortran - Evaluation Exercises

Last revision: March, 2022

---

These programs were the last evaluation exercises for the course *TÉCNICAS COMPUTACIONALES Y CÁLCULO NUMÉRICO* in the EMTCCM.

- **Author:** Rony J. Letona
- **email:** [rony.letona@estudiante.uam.es](mailto:rony.letona@estudiante.uam.es)

---

## 1. Installation

### a) Required software

These programs have been compiled and tested using **GFortran** and **Python**. Therefore, we suggest that you compile the source code using GFortran in a unix-like environment. If you don't have such an environment, please have a look at the [Installing GFortran](https://fortran-lang.org/learn/os_setup/install_gfortran) article in the Fortran official page. Additionally, to run the Python scripts (some Fortran programs require a Python script), please have a look at the [Installation](https://docs.anaconda.com/anaconda/install/index.html) article from Anaconda to set up your environment.

To check if you have GFortran and Python installed in your unix-like environment, please run the following command in a Terminal:

`which gfortran`

`which python`

If the result isn't a path, please install GFortran using one of the following:

- Debian-based Linux: `sudo apt-get install gfortran`
- RedHat-based Linux: `sudo yum install gcc-gfortran` or `sudo dnf install gcc-gfortran`
- Arch Linux: `sudo pacman -S gcc-fortran`
- macOS: (if you have XCode installed) `xcode-select --install` otherwise `port search gcc && sudo port install gcc10`

And in the case of Python, if you don't want to install Anaconda, please make sure you have the base installation and the required libraries:

- Debian-based Linux: `sudo apt-get install python`
- RedHat-based Linux: `sudo yum install python` or `sudo dnf install python`
- Arch Linux: `sudo pacman -S python`
- macOS: This requires some steps:
  1. `/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"`
  2. `echo 'export PATH="/usr/local/opt/python/libexec/bin:$PATH"' >> ~./profile`
  3. `brew install python`

The libraries are somewhat easier to install once you have the Python base.

- Python base installation:
  1. `pip install numpy`
  2. `pip install scipy`
  3. `pip install matplotlib`
- Anaconda installation:
  1. `conda install -c anaconda numpy`
  2. `conda install -c anaconda scipy`
  3. `conda install -c conda-forge matplotlib`

### b) Source code

The source code for this program should be included with this README, and it should be comprised of the following:

- **Calculus**\
  |- **Integration**\
  &nbsp;&nbsp;&nbsp;&nbsp;|- **Simpson**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- imethods.f90\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- simpson.f90\
  &nbsp;&nbsp;&nbsp;&nbsp;|- **Romberg**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- imethods.f90\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- romberg.f90\
  &nbsp;&nbsp;&nbsp;&nbsp;|- **Gauss-Legendre**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- GaussLegendre.f90\
  |- **Optimization**\
  &nbsp;&nbsp;&nbsp;&nbsp;|- **Steepest Descent**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- dmethods.f90\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- SD-2D.f90\
  &nbsp;&nbsp;&nbsp;&nbsp;|- **Newton-Raphson**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- dmethods.f90\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- NR-2D.f90

- **Linear Algebra**\
  |- **Huckel**\
  &nbsp;&nbsp;&nbsp;&nbsp;|- iomethods.f90\
  &nbsp;&nbsp;&nbsp;&nbsp;|- lamethods.f90\
  &nbsp;&nbsp;&nbsp;&nbsp;|- huckel.f90\
  &nbsp;&nbsp;&nbsp;&nbsp;|- density_plotter.py\
  |- **Least Squares**\
  &nbsp;&nbsp;&nbsp;&nbsp;|- iomethods.f90\
  &nbsp;&nbsp;&nbsp;&nbsp;|- lamethods.f90\
  &nbsp;&nbsp;&nbsp;&nbsp;|- LS.f90\
  &nbsp;&nbsp;&nbsp;&nbsp;|- plotter.py


The additional files and directories are used to compile the program, explain how to install and run the program.

- **Calculus**\
  |- **Integration**\
  &nbsp;&nbsp;&nbsp;&nbsp;|- README.md\
  &nbsp;&nbsp;&nbsp;&nbsp;|- **Simpson**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- makefile\
  &nbsp;&nbsp;&nbsp;&nbsp;|- **Romberg**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- makefile\
  &nbsp;&nbsp;&nbsp;&nbsp;|- **Gauss-Legendre**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- makefile\
  |- **Optimization**\
  &nbsp;&nbsp;&nbsp;&nbsp;|- README.md\
  &nbsp;&nbsp;&nbsp;&nbsp;|- **Steepest Descent**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- makefile\
  &nbsp;&nbsp;&nbsp;&nbsp;|- **Newton-Raphson**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- makefile

- **Linear Algebra**\
  |- README.md\
  |- **Huckel**\
  &nbsp;&nbsp;&nbsp;&nbsp;|- makefile\
  &nbsp;&nbsp;&nbsp;&nbsp;|- **mols**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- benzene.xyz\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- butadiene.xyz\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- fullerene.xyz\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- naphtalene.xyz\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- pyrene.xyz\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- MolOpt.7z\
  &nbsp;&nbsp;&nbsp;&nbsp;|- **Reference**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- **benzene**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- **butadiene**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- **fullerene**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- **naphtalene**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- **pyrene**\
  |- **Least Squares**\
  &nbsp;&nbsp;&nbsp;&nbsp;|- makefile\
  &nbsp;&nbsp;&nbsp;&nbsp;|- **Tests**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- data3_1.dat\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- data3_2.dat\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- data3_3.dat\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- data3_4.dat\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- data2_5.dat\
  &nbsp;&nbsp;&nbsp;&nbsp;|- **PythonTests**\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- regression.py\
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|- test.py

### c) GFortran and Make versions

Before compiling the program, it would be a good idea to check the version of GFortran, Python and Make. This program was compiled and tested using GFortran 9.3.0, Python 3.8.10 and GNU Make 4.2.1. To check your GFortran and Make versions, please run the following commands in a Terminal.

`gfortran --version`

`python --version`

`make --version`

### d) Installation

Finally, if you have GFortran installed in your system, and the version is comparable, then please open a Terminal window in the folder with the specific exercise and run the following command:

`make`

Please note that the only files in that folder should be the ones listed in section 1. b) of this README. Otherwise, *make* may fail to compile the program.

## 2. Running each program

Each program is an exercise, but it requires to be executed separately. Therefore, to avoid any confusion, every collection of programs has a separate README file in its own folder.

For more information on how to run each program, please refer to that file.

## 3. Acknowledgements

Special thanks go to *Laura Sánchez Muñoz* for the motivation, the laughs, and the insight. I couldn't have finished several parts of these without you. *Y recuerda, ... eres genial!*

Also, thanks to *Cristina López Cava*, *David Varas*, *José Manuel González* and *Sergi Betkhoshvili* for the revisions, the feedback and ideas; you guys deserve the best of the best.

## 4. License

Copyright 2022 Rony J. Letona

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.