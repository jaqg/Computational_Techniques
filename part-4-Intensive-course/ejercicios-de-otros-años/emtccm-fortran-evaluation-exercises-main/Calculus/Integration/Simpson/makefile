# Makefile for Numerical Integration Homework 2: Simpson Method

simpson: imethods.o simpson.o
	gfortran simpson.o imethods.o -o simpson.exe
	chmod +x simpson.exe
	mkdir source && mv *.f90 ./source/
	rm -f *.o
	rm -f *.mod

simpson.o: simpson.f90
	gfortran -c simpson.f90 -o simpson.o

imethods.o: imethods.f90
	gfortran -c imethods.f90 -o imethods.o

.PHONY: clean

clean:
	mv source/*.f90 ./
	rmdir source
	rm *.exe
