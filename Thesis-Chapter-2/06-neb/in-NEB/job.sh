#!/bin/sh

n=7;

mpirun -np $n lmp_cc -partition ${n}x1 -in in.neb.hop

# output the energy
cat /dev/null > log-neb-energy.dat;
cat /dev/null > neb.xyz;
for ((i=0;i<$n;i++));
do
  grep -B 1 'Loop' log.lammps.$i | tail -n 2 | head -n 1 | awk '{printf ("%12.5f\n",$3)}' >> log-neb-energy.dat;
done; 
for ((i=1;i<$n;i++));
do
  tail -n 67 dump.neb.$i >> neb.xyz;
done; 

Es1=`head -n 1 log-neb-energy.dat | awk '{printf $1}'`
Es2=`tail -n 1 log-neb-energy.dat | awk '{printf $1}'`
E_saddle=`sort -n log-neb-energy.dat | tail -n 1 | awk '{printf $1}'`

echo $Es1 $Es2 $E_saddle > tmp1;
dEs1=`awk '{printf ("%12.5f",$3-$1)}' tmp1`
dEs2=`awk '{printf ("%12.5f",$3-$2)}' tmp1`
echo $dEs1 $dEs2 > barrier.dat;
rm tmp1;
