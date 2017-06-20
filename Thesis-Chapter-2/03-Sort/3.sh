#!/bin/sh

cp ../02-Eb/out-Eb-opt.dat .;

nLi=`head -n 1 ../02-Eb/out-Li.xyz | awk '{printf $1}'`
E_Si=`grep -B 1 'Loop' ../00-min/log.lammps | head -n 1 | awk '{printf $2}'`
# E_Li=`head -n 1 ../00-min/c-Li/log | awk '{printf $1}'`
E_Li=0.0;
Eb_cut=0.0;

# write option file
echo "# n_pos" > option;
echo $nLi >> option;
echo "# Energy of a-Si E_Si, E_Li" >> option;
echo $E_Si $E_Li >> option;
echo "# Eb energy cut" >> option;
echo $Eb_cut >> option;

./Sort
