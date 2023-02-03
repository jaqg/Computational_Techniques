#! /bin/bash
# |-------------------------------------------------|
# | Autor: José Antonio Quiñonero Gris              |
# | Fecha de creacion: Tuesday 12:58:10 04-10-2022 |
# |_________________________________________________|
# Script name:
# Description:

ejercicio = "exercise13"
modulo = "module1"

clear
gfortran -c $modulo.f90
gfortran -c $ejercicio.f90
gfortran -std=f95 -o $ejercicio.x $modulo.o $ejercicio.o
./$ejercicio.x
