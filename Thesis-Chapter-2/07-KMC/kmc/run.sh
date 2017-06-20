#!/bin/sh

#---- Define variable ----
n_step=1000000;  # 1e6
n_config=1;
v=1e13;
kB=8.6e-5;
T=300;
dG0=0.55;
# alpha=1.0;
alpha=0.0;
n_out=1000;
#-------------------------

date > time;

echo "# n_step" > option;
echo ${n_step} >> option;
echo "# v(/s) kB(eV/K), Temp(K), dG0(eV), alpha" >> option;
echo ${v}  ${kB}  ${T}  ${dG0}  ${alpha} >> option;
echo "# n_out" >> option;
echo ${n_out} >> option;
mv option INPUT;

cd INPUT;
#cp option position.dat connect.dat structures.dat Periodic-box.dat  ../RW;
cp option position.dat connect5.dat structures.dat Periodic-box.dat  ../RW;
cd ../RW;
rm out* Final*;
cp position.dat position-0.dat;
./RW;
cd ..;

rm -r OUT-MSD;
mkdir OUT-MSD;
rm -r OUT-Tot-MSD;
mkdir OUT-Tot-MSD;
cp RW/out-Tot-MSD.dat OUT-Tot-MSD/0.dat;

# for ((i=1;i<=${n_config};i++));
# do
#   echo "---- Config = "${i}" ----";
#   cd RW;
#   cp Final-position.dat position.dat;
#   cp Final-Periodic-box.dat Periodic-box.dat;
#   ./RW;
#   cp out-MSD.dat ../OUT-MSD/${i}.dat;
#   cp out-Tot-MSD.dat ../OUT-Tot-MSD/${i}.dat;
#   cd ..; 
#   echo "-------- End ------------";
# done

date >> time;
