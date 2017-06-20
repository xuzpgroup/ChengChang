#!/bin/sh

cp ../../06-neb/connect5.dat kmc/INPUT/;
cp ../../06-neb/structures.dat kmc/INPUT/;
cp ../../05-CONNECT/position.dat kmc/INPUT/;
cp ../../06-neb/structures.dat .;

nLi=`head -n 2 structures.dat | tail -n 1 | awk '{printf $1}'`
for ((i=1;i<$nLi;i++))
do
  mkdir $i;
  cp -r kmc/* $i;
  cd $i;
  cd INPUT;
  
  # write Periodic-box.dat
  echo "Time(s)" > tmp1;
  echo 0.0 >> tmp1;
  echo "# Particle NO, nx, ny, nz" >> tmp1;
  for ((j=1;j<=i;j++));
  do
    echo "    "$j"    "0"    "0"    "0 >> tmp1;
  done; 
  cp tmp1 Periodic-box.dat;

  # write position.dat 
  head -n 3 position.dat > tmp2;
  echo $i >> tmp2;
  head -n 5 position.dat | tail -n 1 >> tmp2;
  tail -n $nLi position.dat | head -n $i > tmp3 ;
  awk '{printf ("%5d%5d%12.5f\n",$1,$1,$3)}' tmp3 >> tmp2;
  tmp_n=`head -n 4 tmp2 | tail -n 1 | awk -v var1=$nLi '{printf ("%5d",var1-$1)}'`
  tail -n $tmp_n position.dat | awk '{printf ("%5d%5d%12.5f\n",$1,$2,$3)}' >> tmp2
  cp tmp2 position.dat;

  rm tmp*;

  cd ..;
  cd ..;
done;
