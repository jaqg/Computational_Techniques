#! /bin/bash
# |-------------------------------------------------|
# | Autor: José Antonio Quiñonero Gris              |
# | Fecha de creacion: Tuesday 12:58:10 04-10-2022  |
# |_________________________________________________|
# Script name:
# Description:

clear
gfortran -c successions_mod.f90
gfortran -c main.f90
gfortran -o main successions_mod.o main.o
./main