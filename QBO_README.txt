
DATA PROCESSING OF RADIOSONDE DATA
==================================

I. Overview
###########

1.) EXTRACT SINGAPORE
---------------------
To generate the monthly QBO time series currently the original Singapore
radiosonde soundings are used, as they are distributed via the WMO global
telecommunication system. All available soundings for 00 and 12 UTC are 
used, decoded and error checked.

 - done with sub-scripts:
   generate.sh
   parse.sh

The procedure to interpolate for data at 10 hPa for the early data is 
currently unknown.

The wind information for the main level 100.0, 70.0, 50.0, 30.0, 20.0, 
and 10.0 hpa (if available) are directly decoded from the sounding.
In addition to these main pressure levels, there are some significant 
points encoded in the sounding, that give a lot of additional, 
vertically better resolved wind information. As these additional 
information are on non standard pressure levels, and change from sounding 
to sounding, interpolation to a set of standard pressure levels is needed.
 
2.) INTERPOLATION
---------------------
The interpolation to the final pressure levels 
100, 90, 80, 70, 60, 50, 45, 40, 35, 30, 25, 20, 15, 12, and 10 hpa 
is done linear in p**(R_d/c_p), i.e. p**0.286. 

The interpolated data is than averaged to get the final monthly 
mean wind data. 

 - done with sub-script:
   interpolate.sh

3.) PLOTTING
--------------
Currently the plotting is done with R.
See folder qbo_Rplot, script qbo_plot.R.

4.) Date for web-page
-----------------------
The data files for the web-page are stored in the folder srv.
Currently these are:
  qbo.dat           -  low resolution QBO since 1953
  singapore.dat     -  high resolution QBO since 1987
  singapore2021.dat -  high resolution QBO for the current year (2021)
  singapore2020.dat -  high resolution QBO for the last year (2020)
  qbo_data_tgz.tgz  -  compressed tar file containing current data files
  qbo_data_zip.zip  -  zip-file containing current data files

After updating the QBO to the current month, the files singapore2021.dat
and singapore2020.dat have to e replaced by singapore2023.dat and singapore2022.dat.


II. QBO-WORKFLOW
################

QBO.sh - Start script

With this script the following sub-scripts are executed for one single month.
 - generate.sh
 - parse.sh
 - interpolate.sh
 - file_add (file_add.f90)

All available data for this month are processed. Execute when the month is finished.

generate.sh
 -> gen_indata (gen_indata.f90)
    Extract and gather the coded message for station Singapore from the original
    data files for 00 and 12 UTC.
    The original data files are available separatly for each section A/C/D in the folder
     ~/wmo_archiv/<yyyy>/M<mm>
    Files (e.g.):
     afw2ha.<dd>00 afw2hc.<dd>00 afw2hd.<dd>00
     afw2ha.<dd>12 afw2hc.<dd>12 afw2hd.<dd>12

    Results are stored in folder:
     qbo_data/<yyyy>/M<mm>/
    Files:
     sing<yyyy><mm><dd>.00
     sing<yyyy><mm><dd>.12


parse.sh
 -> parse_wind (parse_wind.f90)
    Decode the ing<yyyy><mm><dd>.<hh> files.
    Generates one output file per month:
    Folder:
     qbo_data/<yyyy>/M<mm>
    File:
     wind<yyyy><mm>
     -> contains for all available soundings entries as:
     D<yyyy><mm><dd>.<hh>
     press(z)[hpa] direction(z)[degree] velocity(z)[knots]

    The first entries are on 6 standard pressure levels.
    All data lines are on uneven pressure levels varying from
    sounding to sounding.

interpolate.sh
 -> interpolate_qbo (interpolate_qbo.f90)
    Interpolate the data of file wind<yyyy><mm> to 21 main pressure levels.
    Generates two output files per month:
    Folder:
     qbo_data/<yyyy>/M<mm>
    File:
     tmp.dat
     -> contains one single line with the monthly zonal wind data on
        the main pressure levels
     wind<yyyy><mm>.txt
     -> contains for all available soundings entries as:
     E.g.

     First block of data contain original pressure level of sounding
     and the zonal and meridional wind components.

     Second block of data contain zonal and meridional wind components
     interpolated to the 21 main pressure levels.

     Last section give the monthly mean data for the 21 main pressure
     levels.

     <dd>.<mm>.<yyyy>  <hh> Uhr

     99.9  -0.8  -3.0   94.6  -9.3  -6.5   93.9  -9.7  -6.8   86.4  -3.3  -9.2
     80.3  -8.9 -10.6   75.2 -17.9  -1.6   70.6 -13.9   9.7   70.0 -13.1   9.1
     66.2 -11.8   8.3   64.1 -12.2   2.1   62.2 -10.8   0.9   60.4 -11.1   4.0
     57.8 -11.8   0.0   56.0 -12.2   2.1   52.9 -13.4  -3.6   50.0 -15.9  -1.4
     47.5 -14.7  -2.6   45.2 -16.4   4.4   38.8 -21.6   0.0   34.2 -24.7 -11.5
     32.4 -35.5   0.0   30.0 -30.9   5.4   27.5 -34.3   3.0   26.1 -26.6  -2.3
     23.9  -6.3   5.3   23.6  -5.0   7.2   22.9  -9.1   9.1   20.8  -0.8   4.6
     20.0  -2.6   4.5   19.5  -3.0   3.5   19.2  -3.5   0.6   18.7  -3.0  -6.5
     18.3   2.1  -8.0   17.4   9.8  -5.7   16.8  14.0  -6.5   15.3   9.7  -0.9
     14.3   4.6  -5.5   13.8   5.1  -4.3   13.5   3.3  -3.9   11.8   3.1  -5.3
     11.5   7.1  -6.0   11.0   4.4  -7.6   10.6   6.2  -6.2   10.0   3.6  -6.2
      9.7   1.9  -5.3    9.1  12.7  -2.2    8.1  13.9  -3.7    7.2   8.4   2.3
      6.6   7.2   0.6    6.3  13.9   3.7

     99.9  -0.8  -3.0   90.0  -6.4  -8.0   80.0  -9.4 -10.1   70.0 -13.1   9.1
     60.0 -11.2   3.4   50.0 -15.9  -1.4   45.0 -16.6   4.3   40.0 -20.6   0.9
     35.0 -24.2  -9.4   30.0 -30.9   5.4   25.0 -16.6   1.4   22.5  -7.6   8.3
     20.0  -2.6   4.5   15.0   8.2  -2.2   12.0   3.1  -5.2   10.0   3.6  -6.2
      8.0  13.3  -3.1    7.0   8.0   1.7    6.0   0.0   0.0    5.0   0.0   0.0
      4.0   0.0   0.0

     ...

     Monatsmittelwind fuer den  1 2006

     Niveau Anzahl     U     V   DD FF
      [hPa]        [m/s] [m/s]     [kn]
       99.9     54 -18.4   3.5  101 36
       90.0     54 -10.3  -1.0   84 20
       80.0     54  -4.5  -1.2   75  9
       70.0     54  -1.4  -0.7   62  3
       60.0     53  -3.9  -0.0   90  8
       50.0     53 -15.2   0.4   91 30
       45.0     52 -21.6   0.2   91 42
       40.0     52 -27.1   0.1   90 53
       35.0     52 -30.3   0.0   90 59
       30.0     52 -28.8   1.2   92 56
       25.0     52  -3.6   0.1   91  7
       22.5     51   4.2  -0.3  274  8
       20.0     51   9.5  -1.0  276 18
       15.0     47  16.2   0.6  268 31
       12.0     47  17.4   0.6  268 34
       10.0     46  17.1  -0.7  272 33
        8.0     26  13.0  -1.3  276 25
        7.0     18   9.2  -1.3  278 18
        6.0      6   7.9   3.3  247 17
        5.0      2   3.5   0.7  259  7
        4.0      0   0.0   0.0    0  0

file_add (file_add.f90)
  -> read in tmp.dat and add the data for this month to the two qbo data files:
     qbo.dat
     singapore.dat



