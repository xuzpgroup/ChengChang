#!/bin/sh

date > time;

rm out-Eb.dat;
rm out-Eb-opt.dat;
sed -n "1,79p" input.data > TMP1.dat;

for i in {1..482};
do
  sed -n "${i}p" out-Li.dat > TMP2.dat;
  awk -v tmp1=65 -v tmp2=1 '{printf("%5d%5d%12.5f%12.5f%12.5f",tmp1,tmp2,$2,$3,$4)}' TMP2.dat > TMP3.dat
  cat TMP1.dat TMP3.dat > TMP4.dat;
  cp TMP4.dat  input.data;
  lmp_cc -in opt.in;
  grep -A 1 'Step' log.lammps | tail -n 1 > TMP5.dat;
  awk -v tmp3=$(awk '{printf $2}' TMP5.dat) '{printf("%12.5f%12.5f%12.5f%12.5f\n",$2,$3,$4,tmp3)}' TMP2.dat >> out-Eb.dat;
  grep -B 1 'Loop' log.lammps | head -n 1 > TMP6.dat; 
  tail -n 1 min.xyz > TMP7.dat;
  awk -v tmp4=$(awk '{printf $2}' TMP6.dat) '{printf("%12.5f%12.5f%12.5f%12.5f\n",$2,$3,$4,tmp4)}' TMP7.dat >> out-Eb-opt.dat;
  rm TMP2.dat TMP3.dat TMP4.dat TMP5.dat TMP6.dat TMP7.dat; 
done;

rm TMP*.dat;

date >> time;
