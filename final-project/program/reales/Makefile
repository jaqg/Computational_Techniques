#
# Makefile
# Jose Antonio Quiñonero Gris
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
MODPATH=modules
MODSRC=$(shell ls $(MODPATH)/*.f90)
MODOBJ=${MODSRC:.f90=.o}
MODMOD=${MODSRC:.f90=.mod}
#
SRC=$(SUBSRC) $(MODSRC) $(MODMOD) $(MAINPROG)
OBJ=${SRC:.f90=.o}
#
AR=ar rcs
LIB=mylib
LIBPATH=./
#
GRAPHDIR=graph
GRAPHPROG=$(GRAPHDIR)/graph.py
ALLGRAPHS=$(shell ls $(GRAPHDIR)/*.py)
GRAPHDATA=$(GRAPHDIR)/out-graph.dat

#
# Main compilation
#
%.o: %.f90 $(MODMOD)
	$(FC) $(FFLAGS) -o $@ -c $< -J $(MODPATH)
#
# Create main executable
#
$(MAINEX): $(OBJ) $(LIB)
	$(FC) $(FFLAGS) -o $@ $(MODOBJ) $(MAINOBJ) -L./$(LIBPATH) -l$(LIB)
#
# Compile modules
#
$(MODMOD): $(MODSRC)
	$(FC) $(FFLAGS) -o $(MODOBJ) -c $< -J $(MODPATH)
#
# `make lib` to create/update library
#
$(LIB): $(SUBOBJ)
	$(AR) lib$(LIB).a $^
#
# `make plot` to create/update the plot
# `make allplots` to create/update all plots
# `make cleanplots` to remove pdf plots
#
plot: $(GRAPHDATA)
	python3 $(GRAPHPROG)
allplots: $(GRAPHDATA)
	@for i in $(ALLGRAPHS); do \
		echo "python3 $$i"; \
		python3 $$i ; \
	done
cleanplots:
	@rm -f $(GRAPHDIR)/*.pdf
#
# Clean
#
clean:
	@rm -f lib$(LIB).a *.mod *.o $(SUBOBJ) main
