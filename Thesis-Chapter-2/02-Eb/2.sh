#!/bin/sh

cp ../01-Li/out-Li.dat .;
cp ../01-Li/out-Li.xyz .;
cp ../01-Li/a-Si.dat .;

# write input.data by using a-Si.dat
nSi=`head -n 1 a-Si.dat | awk '{printf $1}'`;
xlo=`head -n 3 a-Si.dat | tail -n 1 | awk '{printf $1}'`;
xhi=`head -n 3 a-Si.dat | tail -n 1 | awk '{printf $2}'`;
ylo=`head -n 3 a-Si.dat | tail -n 1 | awk '{printf $3}'`;
yhi=`head -n 3 a-Si.dat | tail -n 1 | awk '{printf $4}'`;
zlo=`head -n 3 a-Si.dat | tail -n 1 | awk '{printf $5}'`;
zhi=`head -n 3 a-Si.dat | tail -n 1 | awk '{printf $6}'`;
nSi2=$(echo "$nSi+1"|bc)
echo "One Li in a-Si" > tmp1;
echo "" >> tmp1;
echo "    "$nSi2" atoms" >> tmp1;
echo "     2 atom types" >> tmp1;
echo "    "$xlo"  "$xhi"  xlo xhi" >> tmp1;
echo "    "$ylo"  "$yhi"  ylo yhi" >> tmp1;
echo "    "$zlo"  "$zhi"  zlo zhi" >> tmp1;
echo "" >> tmp1;
echo "    Masses" >> tmp1;
echo "" >> tmp1;
echo "    1  6.941" >> tmp1;
echo "    2  28.0855" >> tmp1;
echo "" >> tmp1;
echo "    Atoms" >> tmp1;
echo "" >> tmp1;
tail -n $nSi a-Si.dat | awk '{printf ("%5d%5d%12.5f%12.5f%12.5f\n",NR,$1,$2,$3,$4)}' >> tmp1;
cp tmp1 input.data;


