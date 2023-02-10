# Makefile for Numerical Integration Homework 2: Romberg Method

romberg: imethods.o romberg.o
	gfortran romberg.o imethods.o -o romberg.exe
	chmod +x romberg.exe
	mkdir source && mv *.f90 ./source/
	rm -f *.o
	rm -f *.mod

romberg.o: romberg.f90
	gfortran -c romberg.f90 -o romberg.o

imethods.o: imethods.f90
	gfortran -c imethods.f90 -o imethods.o

.PHONY: clean

clean:
	mv source/*.f90 ./
	rmdir source
	rm *.exe
