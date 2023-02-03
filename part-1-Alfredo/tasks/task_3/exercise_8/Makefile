# Jose Antonio Quinonero Gris 17/10/2022
#
# Makefile
#
FC=gfortran
FFLAGS=-O2 -std=legacy
SRC=matrices_mod.f90 exercises_5-6.f90
OBJ=${SRC:.f90=.o}

%.o: %.f90
	$(FC) $(FFLAGS) -c $(SRC)

exercises_5-6: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

clean:
	@rm -f *.mod *.o exercises_5-6
