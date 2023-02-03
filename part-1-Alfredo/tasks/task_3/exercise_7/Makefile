#
# Makefile
#
FC=gfortran
FFLAGS=-O2 -Wall -Wextra -std=f2008
# FFLAGS=-O2 -std=f2008
# FFLAGS=-O3 -Wall -Wextra
SRC=modules/patients_mod.f90 modules/matrices_mod.f90 main.f90
OBJ=${SRC:.f90=.o}
MODPATH=modules/
MYLIB=mylib
MYLIBPATH=modules/

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $< -J $(MODPATH)

main: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ) -L./$(MYLIBPATH) -l$(MYLIB)

clean:
	@rm -f *.mod *.o main
