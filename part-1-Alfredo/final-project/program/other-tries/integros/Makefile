#
# Makefile
#
FC=gfortran
FFLAGS=-O2 -Wall -Wextra -std=legacy
# FFLAGS=-O2 -std=f2008
# FFLAGS=-O3 -Wall -Wextra
#
MAINPROG=main.f90
MAINEX=${MAINPROG:.f90=}
MAINOBJ=${MAINPROG:.f90=.o}
#
SUBPATH=subroutines
SUBSRC=$(shell ls $(SUBPATH)/*.f90)
SUBOBJ=${SUBSRC:.f90=.o}
#
SRC=$(SUBSRC) $(MAINPROG)
OBJ=${SRC:.f90=.o}
#
AR=ar rcs
LIB=mylib
LIBPATH=./

#
# Main compilation
#
%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<
#
# Create main executable
#
$(MAINEX): $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(MAINOBJ) -L./$(LIBPATH) -l$(LIB)
#
# `make lib` to create/update library
#
lib: $(SUBOBJ)
	$(AR) lib$(LIB).a $^
#
# Clean
#
clean:
	@rm -f *.mod *.o $(SUBOBJ) main
