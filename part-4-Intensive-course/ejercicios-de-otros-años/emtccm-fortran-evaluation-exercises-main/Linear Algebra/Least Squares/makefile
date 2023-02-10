# Makefile for Least Squares Method

LS: lamethods.o iomethods.o LS.o
	gfortran LS.o iomethods.o lamethods.o -o LS.exe
	chmod +x LS.exe
	chmod +x plotter.py
	mkdir source && mv *.f90 ./source/
	rm -f *.o
	rm -f *.mod

LS.o: LS.f90
	gfortran -c LS.f90 -o LS.o

lamethods.o: lamethods.f90
	gfortran -c lamethods.f90 -o lamethods.o

iomethods.o: iomethods.f90
	gfortran -c iomethods.f90 -o iomethods.o

.PHONY: clean

clean:
	mv source/*.f90 ./
	rmdir source
	rm *.exe
