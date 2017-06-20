#!/bin/sh

cp ../03-Sort/out-Eb-opt-sort2.dat .;
cp ../01-Li/a-Si.dat .;

rc=1.0;
nLi=`wc -l out-Eb-opt-sort2.dat | awk '{printf $1}'`

# write option
echo "# rc" > option;
echo $rc >> option;
echo "# n_pos" >> option;
echo $nLi >> option;

./Select
