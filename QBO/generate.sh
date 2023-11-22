#!/bin/ksh
#set -vx


typeset -Z2  mm yy
typeset -Z4  yyyy

readonly project_path project_dat
project_bin=${project_path}

gen_indata_exe=${project_bin}/gen_indata

date=$1
if [ -z $date ]; then
    echo "Kein Datum aufgerufen!"
    echo "Bitte Datum angeben im Format: ./in_gen.sh yyyy.mm"
    exit
fi

yyyy=${date%%.+([0-9])}
mm=${date##+([0-9]).}

yy=$yyyy

obs_path=${obs_base_path}/${yyyy}/M${mm}

data_path=${project_dat}/${yyyy}/M${mm}


if [ ! -d ${data_path} ] ;then
  mkdir -p ${data_path};
fi

if [ -d ${obs_path} ]; then
  cd ${obs_path}

  filelist=$(ls afw2h[acd].??[01][02])
  if [ ${#filelist} == 0 ]; then
    compressed_archive=True
    uncompress_dir=${HOME}/tmp/Stationsdaten_DWD/wmo_archiv/${yyyy}/M${mm}
    if [ ! -d ${uncompress_dir} ]; then
      mkdir -p ${uncompress_dir}
    fi
    filelist=$(ls afw2h[acd].??[01][02].gz)
    cd ${uncompress_dir}
  else
    compressed_archive=False
    filelist=$(ls afw2h[acd].??[01][02])
  fi
  #echo $filelist
  echo "Kopiere RS Daten aus dem Archiv"

  for files in $filelist; do
    if [ ${compressed_archive} == True ]; then
      # uncompress data
      if [ ! -f $(basename ${files} .gz) ]; then
        cp -p ${obs_path}/${files} .
        gunzip ${files}
      fi
      work_file=$(basename ${files} .gz)
      work_dir=${uncompress_dir}
    else
      work_file=${files}
      work_dir=${obs_path}
    fi
    #date=${yyyy}${mm}$(expr substr "$(basename ${work_file})" 8 4)
    date=${yyyy}${mm}$(expr "$(basename "$work_file")" : '.\{7\}\(.\{4\}\)')
    #echo $date
    #echo ${gen_indata_exe} ${work_file} ${work_dir} ${data_path} ${date}
    ${gen_indata_exe} ${work_file} ${work_dir} ${data_path} ${date}
  done
  if [ ${compressed_archive} == True ]; then
    rm -r ${uncompress_dir}
  fi

else
  echo "data path ${obs_path} not existing."
fi

