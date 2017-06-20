#!/bin/sh

cp ../04-Select/out-Eb-opt-select.dat .;
n_pos=`wc -l out-Eb-opt-select.dat | awk '{printf $1}'`
rc=5.0;
echo "rc = "$rc;

# write structures.dat
echo "# n_pos" > tmp1;
echo $n_pos >> tmp1;
echo "# criteria for connectivity, rc" >> tmp1;
echo $rc >> tmp1;
echo "# xlo xhi ylo yhi zlo zhi" >> tmp1;
head -n 3 ../01-Li/a-Si.dat | tail -n 1 >> tmp1;
echo "# Coordinate X, Y, Z, Binding Energy(eV)" >> tmp1;
tail -n $n_pos  out-Eb-opt-select.dat >> tmp1;
cp tmp1 structures.dat;
rm tmp1

./CONNECT
