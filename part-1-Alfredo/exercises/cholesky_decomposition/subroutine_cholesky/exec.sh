#! /bin/bash
# +-------------------------------------------------+
# | Autor: José Antonio Quiñonero Gris              |
# | Fecha de creacion: Wednesday 17:31:19 05-10-2022 |
# +-------------------------------------------------+

principal='main'
modulo='cholesky_mod'

clear
# echo "Compilando $modulo.f90"
gfortran -c $modulo.f90
# echo "Compilando $principal.f90"
gfortran -c $principal.f90
# echo "Creando ejecutable $principal"
gfortran -std=f95 -o $principal $modulo.o $principal.o
# echo "Ejecuantado $principal"
./$principal
# echo "Fin"
