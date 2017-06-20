#!/bin/sh

lmp_cc -in opt.in;
nSi=`head -n 1 ../a-Si.dat | awk '{printf $1}'`;
echo $nSi > a-Si.dat;
echo "lattice vector" >> a-Si.dat;
grep -B 1 'Loop' log.lammps | head -n 1 | awk '{printf ("%12.5f%12.5f%12.5f%12.5f%12.5f%12.5f\n",$4,$5,$6,$7,$8,$9)}' >> a-Si.dat;
echo "atoms" >> a-Si.dat;
tail -n $nSi min.xyz | awk '{printf ("%5d%12.5f%12.5f%12.5f\n",$1,$2,$3,$4)}' >> a-Si.dat;
