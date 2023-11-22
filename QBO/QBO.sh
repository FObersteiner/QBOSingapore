#!/bin/ksh
obs_base_path=/Users/tobias/Git/QBOSingapore/wmo_archiv
project_dat=/Users/tobias/Git/QBOSingapore/Test
project_path=/Users/tobias/Git/QBOSingapore/QBO/bin

typeset -Z2  mm yy
typeset -Z4  yyyy

qbo_data=./qbo_data

wdir=$(pwd)

export project_path=${wdir}/bin
export project_dat=${wdir}/${qbo_data}

#export obs_base_path=/daten/opdata/Stationsdaten_DWD/wmo_archiv
export obs_base_path=${HOME}/Git/QBOSingapore/wmo_archiv

if [ ! -d ${project_dat} ]; then
   mkdir -p ${project_dat}
fi

set -- `getopt hwga: $@`
echo "Pararmeterliste beim QBO Aufruf: " $@

while
  test "X$1" != "X--"
do
  case "$1"
    in
    -a)     archiv_flag=true
	    date=$2
	    ### Stefan's Version:
	    # yyyy=${date%%.+([0-9])}
		### vereinfacht, da Fehler aufgetreten ist (Franzi Jan2014)
	    yyyy=${date%%.*}
	    mm=${date##+([0-9]).}
	    #echo 'archiv'
	    #echo $yyyy
	    #echo $mm
	    shift 2
	    ;;
    -h)     echo ''
            echo 'Schalter  Bedeutung   Moeglich'
            echo '----------------------------------'
            echo ' -g        generieren der Inputdaten'
            echo ' -w        Daten in qbo.dat, singapore.dat ablegen'
            echo ' Beispiel: QBO -g -w -a 2009.01'
            exit
            ;;
    -g)     generate=true
	    #echo 'generat=true'
            shift
            ;;
    -w)     write_data=true
	    #echo 'write_data=true'
	    shift
            ;;
    *)	    echo "invalid parameter $1" 1>&2
            shift
            fehler=1
            ;;
    
  esac
done


echo ${yyyy}.${mm}
if [ ${generate} ]; then
./generate.sh ${yyyy}.${mm}
fi

./parse.sh ${yyyy}.${mm}
./interpolate.sh ${yyyy}.${mm}

qbo_path=${qbo_data}/${yyyy}/M${mm}
fileadd=${project_path}/file_add

if [ ${write_data} ];then
        #echo 'enter routine add file'
   if [ ! -d ${qbo_data}/backup ]; then
      mkdir ${qbo_data}/backup
   fi
   ${fileadd} ${qbo_path}
   if [ -f ${qbo_data}/qbo.dat ]; then
     cp ${qbo_data}/qbo.dat ${qbo_data}/backup/qbo_backup.dat
     rm ${qbo_data}/qbo.dat
   fi
   if [ -f ${qbo_data}/qbo_cache.dat ]; then
     mv ${qbo_data}/qbo_cache.dat ${qbo_data}/qbo.dat
   fi
   if [ -f ${qbo_data}/singapore.dat ]; then
     cp ${qbo_data}/singapore.dat ${qbo_data}/backup/singapore_backup.dat
     rm ${qbo_data}/singapore.dat
   fi
   if [ -f ${qbo_data}/singapore_cache.dat ]; then
     mv ${qbo_data}/singapore_cache.dat ${qbo_data}/singapore.dat
   fi
fi
