#!/bin/sh

cp ../00-min/a-Si.dat .;

echo "# rc, min distacne between Li and Si" > option
echo 1.0 >> option;
echo "# dx, dy, dz" >> option;
echo 1.0  1.0  1.0 >> option;
./Li;
