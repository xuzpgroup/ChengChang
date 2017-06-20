#!/bin/sh

#cp ../05-CONNECT/connect3.dat .;
cp ../05-CONNECT/connect32.dat .;
cp ../05-CONNECT/structures.dat .;
cp ../02-Eb/input.data .;
cp ../05-CONNECT/out-Eb-opt-select.dat .;

mkdir opt-structure file-input file-final;

nLi=`head -n 2 structures.dat | tail -n 1 | awk '{printf $1}'`

for ((i=1;i<=$nLi;i++));
#for i in 1;
do
  head -n 15 input.data > tmp1;
  num=`head -n $i out-Eb-opt-select.dat | tail -n 1 | awk '{printf $5}'`
  cp ../02-Eb/opt-structure/${num}.xyz .;
  tail -n 65 ${num}.xyz | awk '{printf ("%5d%5d%12.5f%12.5f%12.5f\n",NR,$1,$2,$3,$4)}' >> tmp1;

  # write input.data$i
  cp tmp1 input.data$i
  # write final.hop$i
  echo 65 > final.hop$i;
  tail -n 65 tmp1 | awk '{printf ("%5d%12.5f%12.5f%12.5f\n",NR,$3,$4,$5)}' >> final.hop$i;
  rm tmp1;
  
  mv ${num}.xyz opt-structure;
  mv input.data$i file-input;
  mv final.hop$i file-final;
done;
