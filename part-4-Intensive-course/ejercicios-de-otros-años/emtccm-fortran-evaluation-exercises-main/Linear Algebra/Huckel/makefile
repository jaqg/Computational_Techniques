# Makefile for the Huckel Method

huckel: lamethods.o iomethods.o huckel.o
	gfortran huckel.o iomethods.o lamethods.o -o huckel.exe
	chmod +x huckel.exe
	mkdir source && mv *.f90 ./source/
	rm -f *.o
	rm -f *.mod

huckel.o: huckel.f90
	gfortran -c huckel.f90 -o huckel.o

lamethods.o: lamethods.f90
	gfortran -c lamethods.f90 -o lamethods.o

iomethods.o: iomethods.f90
	gfortran -c iomethods.f90 -o iomethods.o

.PHONY: clean

clean:
	mv source/*.f90 ./
	rmdir source
	rm *.exe