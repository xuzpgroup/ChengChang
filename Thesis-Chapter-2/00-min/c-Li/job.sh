#!/bin/sh

lmp_cc -in opt.in;
ELi=`grep -B 1 'Loop' log.lammps | head -n 1 | awk '{printf $2}'`
nLi=`head -n 1 min.xyz | awk '{printf $1}'`
dELi=$(echo "scale=5; $ELi/$nLi"|bc)
echo $dELi > log;
