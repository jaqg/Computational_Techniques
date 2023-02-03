#
# Makefile
# Author: Jose Antonio Quiñonero Gris
#
# === VARIABLES ===
#
# Select the compiler and flags
#
FC=gfortran
FFLAGS=-O2 -Wall -Wextra -Wno-uninitialized -std=legacy
# FFLAGS=-O2 -Wall -Wextra -std=legacy
# FFLAGS=-O2 -std=f2008
# FFLAGS=-O3 -Wall -Wextra
#
# Specify file name containing the main program in 'MAINPROG'
# The executable (without extension) and object (.o) are defined from it
# (I follow this way of defining objects throughout the Makefile)
#
MAINPROG=main.f90
MAINEX=${MAINPROG:.f90=}
MAINOBJ=${MAINPROG:.f90=.o}
#
# Specify path to the subroutines directory
# Note: all .f90 files in the SUBPATH folder are considered needed subroutines
#
# Specify path to the modules directory
# IMPORTANT NOTE: NAME OF THE MODULE MUST BE THE SAME AS NAME OF FILE
# If it is not the case, change the definition of $(MODMOD) as
# MODMOD=mymodule1.mod mymodule2.mod ...
#
MODPATH=modules
MODSRC=$(shell ls $(MODPATH)/*.f90)
MODOBJ=${MODSRC:.f90=.o}
MODMOD=${MODSRC:.f90=.mod}
#
# I define a "general" variable containing all source files, 'SRC', and so
# the objects in 'OBJ'
#
SRC=$(MODSRC) $(MAINPROG)
OBJ=${SRC:.f90=.o}
#
# === COMMANDS ===
#
# Main compilation of all .f90
#
%.o: %.f90 $(MODMOD)
	$(FC) $(FFLAGS) -o $@ -c $< -J $(MODPATH)
#
# Compile modules
#
%.mod: %.f90
	$(FC) $(FFLAGS) -c $^ -J $(MODPATH)
	@mv ${^F:.f90=.o} $(MODPATH)
#
# Create main executable
#
$(MAINEX): $(OBJ) $(MODMOD)
	$(FC) $(FFLAGS) -o $@ $(MODOBJ) $(MAINOBJ)

clean:
	@rm -f $(MODMOD) $(OBJ) *.o main
