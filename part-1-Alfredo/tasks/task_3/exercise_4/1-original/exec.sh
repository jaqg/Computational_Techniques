clear
echo "rm -f *.mod *.o"
rm -f *.mod *.o
echo "gfortran -c matrices_mod.f90"
gfortran -c matrices_mod.f90
echo "gfortran -c main.f90"
gfortran -c main.f90
echo "gfortran -o main matrices_mod.o main.o"
gfortran -o main matrices_mod.o main.o
echo "./main"
./main
