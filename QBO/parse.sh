#!/bin/ksh

typeset -Z2  mm yy
typeset -Z4  yyyy

readonly project_path project_dat

parse_wind_exe=${project_path}/parse_wind

date=$1
if [ -z $date ]; then
    echo "Kein Datum aufgerufen!"
    echo "Bitte Datum angeben im Format: ./in_gen.sh yyyy.mm"
    exit
fi

yyyy=${date%%.+([0-9])}
mm=${date##+([0-9]).}

yy=${yyyy}
data_path=${project_dat}/${yyyy}/M${mm}
wind_file=${data_path}/wind${yyyy}${mm}

cd ${data_path}
echo 'Parse RS Daten'

if [ -e parse.err ]; then
    rm parse.err
fi

if [ -e ${wind_file} ]; then
    rm ${wind_file}
fi

filelist=$(ls sing*.[01][02])
for files in $filelist; do
   #echo ${files}
   #echo ${data_path}/$files

   ${parse_wind_exe}<<EOF
${data_path}/${files}
${data_path}/wind.tmp
${data_path}/parse.err
EOF

   cat ${data_path}/wind.tmp >> ${wind_file}
   rm  ${data_path}/wind.tmp
done	
