#
# Makefile
#
FC=gfortran
FFLAGS=-O2 -Wall -Wextra -std=f2008
# FFLAGS=-O2 -std=f2008
# FFLAGS=-O3 -Wall -Wextra
SRC=matrices_mod.f90 main.f90
OBJ=${SRC:.f90=.o}

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

main: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

clean:
	@rm -f *.mod *.o main
