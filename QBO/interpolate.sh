#!/bin/ksh

typeset -Z2  mm yy
typeset -Z4  yyyy

readonly project_path project_dat

interpolate_qbo_exe=${project_path}/interpolate_qbo

date=$1
if [ -z $date ]; then
    echo "Kein Datum aufgerufen!"
    echo "Bitte Datum angeben im Format: ./${0} yyyy.mm"
    exit
fi

yyyy=${date%%.+([0-9])}
mm=${date##+([0-9]).}

yy=${yyyy}
data_path=${project_dat}/${yyyy}/M${mm}
#echo $data_path
wind_file=wind${yyyy}${mm}
#echo ${wind_file}


cd ${data_path}

if [ -e interpol.err ]; then
    rm interpol.err
fi

if [ -e ${wind_file}.txt ]; then
    rm ${wind_file}.txt
fi

if [ -e tmp.dat ]; then
    rm tmp.dat
fi

echo "Interpoliere die QBO aus den RS Daten"

${interpolate_qbo_exe}<<EOF
$data_path/${wind_file}
$data_path/${wind_file}.txt
$data_path/interpol.err
$data_path/tmp.dat
EOF

