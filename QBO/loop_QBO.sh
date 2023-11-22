#!/bin/ksh

typeset -RZ2 SMM EMM IMM

#SYEAR=2006
#EYEAR=2020

SYEAR=2023
EYEAR=2023

#SMM=01
#EMM=12

SMM=01
EMM=09

LOGDIR=log

if [ ! -d ${LOGDIR} ]; then
   mkdir ${LOGDIR}
fi

YEAR=${SYEAR}
while [ ${YEAR} -le ${EYEAR}  ]; do

  IMM=${SMM}
  while [ ${IMM} -le ${EMM}  ]; do
    echo ${YEAR} ${IMM}
    #
    # -g   -> generate
    # -w   -> write data to files qbo.dat and singapore.dat
    #
    ./QBO.sh -g -w -a ${YEAR}.${IMM} > ${LOGDIR}/out_${YEAR}_${IMM}.log
    (( IMM = ${IMM} + 1 ))
  done
  (( YEAR = ${YEAR} + 1 ))
done
