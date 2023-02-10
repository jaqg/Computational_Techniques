# Makefile for Numerical Integration Homework 2: Gauss-Legendre Method

GaussLegendre: SubGauleg.o GaussLegendre.o
	gfortran GaussLegendre.o SubGauleg.o -o GaussLegendre.exe
	chmod +x GaussLegendre.exe
	mkdir source && mv *.f90 ./source/
	rm -f *.o
	rm -f *.mod

GaussLegendre.o: GaussLegendre.f90
	gfortran -c GaussLegendre.f90 -o GaussLegendre.o

SubGauleg.o: SubGauleg.f90
	gfortran -c SubGauleg.f90 -o SubGauleg.o

.PHONY: clean

clean:
	mv source/*.f90 ./
	rmdir source
	rm *.exe
