# QBOSingapore
Extract monthly means of mean zonal wind velocities from singapore radiosonde data 

1. Save file from Singapore and unzip in ```TEMP YYYY-MM``` in the folder ```~/Git/QBOSingapore/wmo_archiv```
2. Create TAC files using the script ```~/Git/QBOSingapore/wmo_archiv/singapore.sh ```
3. Run FUB processing script ```./QBO.sh -g -w -a YYYY.MM``` in folder ```~/Git/QBOSingapore/QBO```
4. Create ```qbo.highres.dat``` by running ```singapore2highres.ipynb``` in the folder ```~/Git/QBOSingapore/data```
5. Complete the file ```qbo_100hPa.dat``` by adding a line with the wind at 100 hPa for the newly added month ```YYYY.MM```.
6. Add 1 for the new month in line 86 ```qbo4.neu <- array(NA, c(442, 16))``` in file ```qbo_plot.R``` in folder ```~/Git/QBOSingapore/qbo_Rplot```.
7. Make new FUB plot by starting ```R``` in folder ```~/Git/QBOSingapore/qbo_Rplot``` and running ```source("qbo_plot.R")```.
8. Copy singapore.dat and qbo.dat from ```~/Git/QBOSingapore/QBO/qbo_data/``` to ```~/Git/QBOSingapore/srv```
9. Update most recent ```singaporeYYYY.dat``` file.
10. Create tar and zip files and cp the FUB plot to server directory by running ```make``` in ```~/Git/QBOSingapore/srv```
11. Create ```jpg``` file by running ```make jpg``` in ```~/Git/QBOSingapore/srv```
12. Create netcdf file of data by running ```qbo_fub2netcdf.ipynb``` in ```~/Git/QBOSingapore/srv```
13. Copy the following files to the webserver: 
    * ```~/Git/QBOSingapore/srv/singapore.dat```
    * ```~/Git/QBOSingapore/srv/singapore2021.dat```
    * ```~/Git/QBOSingapore/srv/singapore2022.dat```
    * ```~/Git/QBOSingapore/srv/singapore2023.dat```
    * ```~/Git/QBOSingapore/srv/qbo.dat```
    * ```~/Git/QBOSingapore/srv/qbo_data_tgz.tgz```
    * ```~/Git/QBOSingapore/srv/qbo_data_zip.zip```
    * ```~/Git/QBOSingapore/srv/qbo_singapore_fub.nc```
    * ```~/Git/QBOSingapore/srv/qbo_wind.jpg```
    * ```~/Git/QBOSingapore/srv/qbo_wind_pdf.pdf```
