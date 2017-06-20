#!/bin/sh

n=`wc -l connect32.dat | awk '{printf $1}'`

cat /dev/null > log.barrier;

for ((k=1;k<=$n;k++));
do
   s1=`head -n $k connect32.dat | tail -n 1 | awk '{printf $1}'`
   s2=`head -n $k connect32.dat | tail -n 1 | awk '{printf $2}'`
   c_ij=`head -n $k connect32.dat | tail -n 1 | awk '{printf $3}'`
   if [ $c_ij = 1 ]; then
     dEs1=`awk '{printf $1}' NEB-$s1-$s2/barrier.dat`; 
     dEs2=`awk '{printf $2}' NEB-$s1-$s2/barrier.dat`; 
     echo $s1 $s2 $dEs1 $dEs2 >> log.barrier;
   else
     dEs1=100.0;
     dEs2=100.0;
     echo $s1 $s2 $dEs1 $dEs2 >> log.barrier;
   fi
done;

awk '{printf ("%5d%5d%12.5f%12.5f\n",$1,$2,$3,$4)}' log.barrier > log.barrier2
awk '{printf ("%5d%12.5f%12.5f\n",1,$3,$4)}' log.barrier > connect4.dat
 
Eb_min=0.01;
Eb_max=2.50;
# echo "# nLi, Eb_min, Eb_max" > option;
# echo $nLi, $Eb_min, $Eb_max >> option;
echo "# n, Eb_min, Eb_max" > option;
echo $n, $Eb_min, $Eb_max >> option;
./Eb2; # output connect5.dat
