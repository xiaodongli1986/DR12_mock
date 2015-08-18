gcc -c sub.c
ifort -c read.f90
ifort -o read sub.o read.o 
