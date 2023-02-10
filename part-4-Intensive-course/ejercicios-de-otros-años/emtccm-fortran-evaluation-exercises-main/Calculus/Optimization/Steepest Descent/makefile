# Makefile for Numerical Integration Homework 1

SD_2D: dmethods.o SD_2D.o
	gfortran SD_2D.o dmethods.o -o SD_2D.exe
	chmod +x SD_2D.exe
	mkdir source && mv *.f90 ./source/
	rm -f *.o
	rm -f *.mod

SD_2D.o: SD_2D.f90
	gfortran -c SD_2D.f90 -o SD_2D.o

dmethods.o: dmethods.f90
	gfortran -c dmethods.f90 -o dmethods.o

.PHONY: clean

clean:
	mv source/*.f90 ./
	rmdir source
	rm *.exe
