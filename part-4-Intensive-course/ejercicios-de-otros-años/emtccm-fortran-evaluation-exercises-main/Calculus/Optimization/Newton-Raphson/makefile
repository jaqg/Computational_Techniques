# Makefile for Numerical Integration Homework 2: Newton-Raphson method

NR_2D: dmethods.o NR_2D.o
	gfortran NR_2D.o dmethods.o -o NR_2D.exe
	chmod +x NR_2D.exe
	mkdir source && mv *.f90 ./source/
	rm -f *.o
	rm -f *.mod

NR_2D.o: NR_2D.f90
	gfortran -c NR_2D.f90 -o NR_2D.o

dmethods.o: dmethods.f90
	gfortran -c dmethods.f90 -o dmethods.o

.PHONY: clean

clean:
	mv source/*.f90 ./
	rmdir source
	rm *.exe
