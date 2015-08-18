#!/bin/bash
#$ -V
#$ -cwd
#$ -S /bin/bash
#$ -N Make_Mock

### Total used process count numbers
#$ -pe cpu_openmpi 1

### Queue Name
### To use cpu => cpu 
### To use gpu => gpu

#$ -q cpu
#$ -R yes
# your job
sh MakeMock.sh
