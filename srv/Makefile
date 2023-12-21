#
# Copy the QBO figu:res from strat25 to the working directory.
# It is necessary to work with idl on strat25.
#
FIGS = ../qbo_Rplot
WDIR = .
REPO = ../QBO/qbo_data/
REPO = .
DATFILES = ${REPO}/qbo.dat ${REPO}/singapore202*.dat ${REPO}/singapore.dat 


#all: cp png gif jpg tar zip
all: cp tar zip jpg

cp:
	cp -p ${FIGS}/qbo_wind_pdf.pdf ${WDIR}
gif:
	convert qbo_wind_pdf.pdf qbo_wind.gif	
#png:
#	convert -density 150 qbo_wind_pdf.pdf -quality 100 -alpha off  qbo_wind.jpg	
png:
	convert -density 150 qbo_wind_pdf.pdf -quality 100 qbo_wind.jpg	
#jpg:
#	convert -density 150 qbo_wind_pdf.pdf -quality 100 -alpha off  qbo_wind.jpg
jpg:
	convert -density 150 qbo_wind_pdf.pdf -quality 100 qbo_wind.jpg

tar:
	tar -czf qbo_data_tgz.tgz $(DATFILES)

zip:
	rm qbo_data_zip.zip
	zip -r qbo_data_zip.zip $(DATFILES)

clean:
	rm qbo_wind.gif qbo_wind.jpg qbo_wind.png *~
